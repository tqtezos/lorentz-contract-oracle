
# require 'json'
require 'yaml'

# No limiting, just trading up at a rate between 0.25 and sqrt 2:

class Sale
  @admin = nil
  class << self
    attr_accessor :admin
  end

  def self.token_name(n)
    "Token#{n}"
  end

  def self.originate_token(admin, n)
    { 'originate' => {
      'alias' => self.token_name(n),
      'config' => '$base-config',
      'command' => 'lorentz-contract print --name ManagedLedger',
      'storage_command' => "lorentz-contract-storage ManagedLedgerBabylon",
      'initial_storage' => {
        'admin' => "$#{self.admin}" },
        'burn_cap' => "4.675" } }
  end

  def self.mint(token, to, value)
    { 'run' => {
        'alias' => "Mint#{token}_#{to}_#{value}",
        'config' => '$base-config',
        'contract' => token,
        'command' => "lorentz-contract-param ManagedLedgerBabylon-mint",
        'parameters' => {
          'to' => "$#{to}",
          'value' => value },
        'burn_cap' => "0.074" } }
  end

  def self.sale_name(held, wanted, held_price, wanted_price)
    "Sale#{held}_#{wanted}_#{held_price}_#{wanted_price}"
  end

  def self.originate_sale(held, wanted, held_price, wanted_price)
  { 'originate' => {
      'alias' => self.sale_name(held, wanted, held_price, wanted_price),
      'config' => '$sale-config',
      'command' => "lorentz-contracts-sale print",
      'storage_command' => "lorentz-contracts-sale init",
      'initial_storage' => {
        'admin' => "$#{self.admin}",
        'held' => "$#{held}",
        'wallet' => "$#{self.admin}",
        'wanted' => "$#{wanted}",
        'held-price' => "#{held_price}",
        'wanted-price' => "#{wanted_price}" },
      'burn_cap' => "1.749" } }
  end

  def self.approve(contract, spender, value)
    { 'run' => {
        'alias' => "Allowance#{admin}_#{spender}_#{value}",
        'config' => "$base-config",
        'contract' => "$#{contract}",
        'command' => "lorentz-contract-param ManagedLedgerBabylon-approve",
        'parameters' => {
          'spender' => "$#{spender}",
          'value' => value },
        'burn_cap' =>  "0.031" }
    }
  end

  def self.binary_tree(depth, number_of_entrants, current_ix=1, y=nil, stage=:originate)
    # raise "originate, then mint, then any other"
    if y
      if depth == 0
      else
        token_size = number_of_entrants * 2**depth
        sale0 = self.token_name(2 * current_ix + 0)
        sale1 = self.token_name(2 * current_ix + 1)
        case stage
        when :originate
          y << self.originate_token(admin, current_ix) if stage == :originate
        when :mint
          y << self.mint(self.token_name(current_ix), self.admin, token_size * 2) if stage == :mint
        when :approve
          y << self.approve(self.token_name(current_ix), self.sale_name(current_ix, sale0, 1, 2), token_size) if stage == :approve
          y << self.approve(self.token_name(current_ix), self.sale_name(current_ix, sale1, 1, 2), token_size) if stage == :approve
        when :sale
          y << self.originate_sale(self.token_name(current_ix), sale0, 1, 2) if stage == :sale
          y << self.originate_sale(self.token_name(current_ix), sale1, 1, 2) if stage == :sale
        else
          raise "unexpected stage: #{stage}"
        end
        self.binary_tree(depth-1, number_of_entrants, 2 * current_ix + 0, y, stage)
        self.binary_tree(depth-1, number_of_entrants, 2 * current_ix + 1, y, stage)
      end
    else
      Enumerator.new do |y|
        self.binary_tree(depth, number_of_entrants, current_ix, y, :originate)
        self.binary_tree(depth, number_of_entrants, current_ix, y, :mint)
        self.binary_tree(depth, number_of_entrants, current_ix, y, :sale)
        self.binary_tree(depth, number_of_entrants, current_ix, y, :approve)
      end
    end
  end

  # def self.from_sales(prng, initial_tokens, &block)
  #   prng.all_individual_sales(initial_tokens, &block).zip(0..1/0.0) do |token_layer, layer|
  #     p [token_layer, layer]
  #     # token_layer.each do |token, xx|
  #     #   p [token, xx]
  #     # token_name = "#{layer}_#{token}"
  #     # self.originate_token(admin, token_name)
  #     # token_size = 100 * total of price numerators
  #     # self.mint(token_name, admin, token_size)
  #     # xx.each do |token_to, price|
  #     #   self.originate_sale(admin, token, token_to, price.numerator, price.denominator)
  #     #   self.approve(token_name, admin, self.sale_name, token_size)
  #     # end
  #   end
  # end
end

class Fixnum
  def num_digits
    self.to_s.length
  end
end

class Bignum
  def num_digits
    self.to_s.length
  end
end

class Rational
  def num_digits
    [self.numerator, self.denominator].map(&:num_digits).max
  end

  def small_digits?
    self.num_digits <= 3
  end
end

class Float
  def small_rs(n=18, m=n)
    (1..n).flat_map do |x|
      self_round_x_r = self.round(x).to_r
      (1..m).map do |y|
        self_round_x_r.truncate(y)
      end.select(&:small_digits?)
    end.uniq.sort_by(&:denominator)
  end
end

class Random
  def self.price_range
    (0.25..2**0.5)
  end

  def choice(arr)
    arr = arr.to_a
    arr[self.rand(0..arr.length-1)]
  end

  def price
    self.choice self.rand(self.class.price_range).small_rs
  end

  def sales(num_tokens, num_next_tokens, num_tokens_total=num_tokens)
    next_token_sales = {}
    unused_next_tokens = (num_tokens_total + 1..num_tokens_total + num_next_tokens).map{|x|[x,true]}.to_h
    while !unused_next_tokens.empty?
      (1..num_tokens).each do |token|
        # next_token_sales = {}
        num_sales = self.rand(1..3)
        (1..num_sales).each do
          next_token_choice = num_tokens_total + self.rand(1..num_next_tokens)
          unused_next_tokens.delete next_token_choice
          next_price = self.price
          if next_token_sales.has_key?(token)
            if next_token_sales[token].has_key?(next_token_choice)
              next_token_sales[token][next_token_choice] << next_price
            else
              next_token_sales[token][next_token_choice] = [next_price]
            end
          else
            next_token_sales[token] = {next_token_choice => [next_price]}
          end
        end
      end
    end
    next_token_sales.sort.map{|k,x|[k,x.sort.to_h]}.to_h
  end

  def all_sales(initial_tokens, y=nil, num_tokens_total=initial_tokens, &block)
    if y
      num_next_tokens = block[initial_tokens]
      if num_next_tokens > 0
        y << self.sales(initial_tokens, num_next_tokens)
        all_sales(num_next_tokens, y, num_tokens_total + num_next_tokens, &block)
      end
    else
      Enumerator.new do |y|
        all_sales(initial_tokens, y, &block)
      end
    end
  end

  def all_individual_sales(initial_tokens, y=nil, &block)
    self.all_sales(initial_tokens, &block).map{|x|x.flat_map{|y,z|z.flat_map{|a,b|b.map{|c|[y,a,c]}}}}
  end

end



# prng = Random.new

# puts JSON.pretty_generate(prng.sales(100, 50)) #.to_a #.to_a.sort_by{|x,_|x}.to_h

# puts prng.all_sales(100){|x| x-10}.map{|x|x.map{|y,z|z.map{|a,b|[y,a,b].inspect}}}

# p prng.all_individual_sales(100){|x|x-10}
# prng.all_individual_sales(100){|x|x-10}.each{|x| p x}
# Sale.from_sales(prng, 100){|x|x-10}

Sale.admin = 'Alice'

# Sale.binary_tree(3, 10, current_ix=1).each{|x| puts JSON.pretty_generate(x)}
# puts YAML.dump({ 'do' => Sale.binary_tree(3, 10, current_ix=1).to_a })
puts YAML.dump({ 'do' => Sale.binary_tree(4, 100).to_a })


# puts "digraph Sales {"
# puts prng.all_sales(100){|x| x-10}.map{|x|x.map{|y,z|z.map{|a,b|[y,a]}}}.flat_map{|x|x.flat_map{|y|y.flat_map{|z,w|"#{z} -> #{w};"}}}.join("\n")
# puts "}"


# (18..18).each do |x|
#   sample = (1..10000).map do
#     prng.rand_price.small_rs(x).length
#   end
#   p [x, sample.reduce(0, :+).to_f / sample.length]
# end
# [18, 4.0056]


# #   _
# rp=prng.rand(0.25..2**0.5);(1..10).map{|x|rp.round(x)}.map{|x|(1..10).map{|y|x.to_r.truncate(y)}}











# class Random
#   @partition_cache = {}

#   class << self
#     attr_accessor :partition_cache
#   end

#   def self.count_partitions(n, limit)
#     if n == 0
#       return 1
#     elsif partition_cache.has_key?([n, limit])
#       return partition_cache[[n, limit]]
#     end
#     x = (1..[limit, n].min + 1).map{|k| self.count_partitions(n-k, k)}.reduce(0, :+)
#     partition_cache[[n, limit]] = x
#     return x
#   end

#   def random_partition(n)
#     a = []
#     limit = n
#     total = Random.count_partitions(n, limit)
#     which = self.rand(total)
#     while n
#       (1..[limit, n].min + 1).each do |k|
#         count = Random.count_partitions(n-k, k)
#         if which < count
#           break
#         end
#         which -= count
#         a << k
#         limit = k
#         n -= k
#       end
#     end
#     a
#   end

#   def partition(n, m)
#     if n == 0
#       []
#     elsif n == m
#       [1] * n
#     else
#       i = self.rand(1..m - (n - 1))
#       self.partition(n-1, m-i) << i
#     end
#   end
# end

# prng = Random.new
# p prng.partition(3, 5)

# class TacoSale
#   attr_accessor :num_tacos # , :num_layers

#   def initialize(num_tacos) # , num_layers)
#     @num_tacos  = num_tacos
#     # @num_layers = num_layers
#   end

#   def make(prng)
#     if self.num_tacos < 2
#       [self.num_tacos]
#     else
#       prng.partition(self.num_tacos, prng.rand(1..self.num_tacos))



