#!/usr/bin/env ruby

require 'open3'
require 'yaml'


def system_lines(cmd)
  Enumerator.new do |y|
    puts cmd
    Open3.popen2e(cmd) do |stdin, stdout, thread|
      while line=stdout.gets do 
        puts(line)
        y << line
      end
      exit_status = thread.value
      unless exit_status.success?
        raise exit_status.inspect
      end
    end
  end
end

def system_line(cmd)
  if result = system_lines(cmd)
    result.first&.chomp
  else
    result
  end
end


module Enumerable
  def find_operation_hash
    self.map do |line|
      line.chomp
    end.select do |line|
      line.match /^Operation hash is '/
    end.map do |line|
      line.sub(/Operation hash is '(.*)'/, '\1')
    end.first
  end

  def pairs(&block)
    self.zip(self.drop(1), &block)
  end

  def find_originated_contract
    self.map do |lines|
      lines.chomp
    end.pairs.select do |x,_|
      x.match /^\s+Originated contracts:/
    end.map do |_,y|
      y.strip
    end.first
  end
end


def system_operation_hash(cmd)
  system_lines(cmd).find_operation_hash
end


class TezosClient
  attr_accessor :tezos_client_path, :user_address

  def initialize(tezos_client_path, user_address)
    @tezos_client_path = tezos_client_path
    @user_address = user_address
  end

  def get_receipt(operation_hash)
    result = nil
    while result.nil?
      result = if result_lines = system_lines("#{@tezos_client_path} get receipt for #{operation_hash}")
        unless result_lines.any?{|line| line.match /Couldn't find operation/}
          result_lines
        end
      end
    end
    result
  end

  def originate_cmd(contract_name, contract_code, initial_storage, burn_cap)
    "#{self.tezos_client_path} --wait none originate contract #{contract_name}" +
    [ "transferring 0 from #{self.user_address} running",
      contract_code.inspect,
      if initial_storage.nil? then "" else "--init #{initial_storage.inspect}" end,
      "--burn-cap #{burn_cap}"
    ].map do |line|
      "  #{line} \\"
    end.join("\n") + "\n  --force"
  end

  def originate(contract_name, contract_code, initial_storage, burn_cap)
    if operation_hash = system_operation_hash(self.originate_cmd(contract_name, contract_code, initial_storage, burn_cap))
      self.get_receipt(operation_hash).find_originated_contract
    end
  end

  def contract_call_cmd(contract_address, parameter, burn_cap)
    "#{self.tezos_client_path} --wait none transfer 0 from #{self.user_address} " +
    [ "to #{contract_address}",
      if parameter.nil? then "" else "--arg #{parameter.inspect}" end,
      "--burn-cap #{burn_cap}"
    ].map do |line|
      "  #{line} \\"
    end.join("\n")
  end

  def contract_call(contract_address, parameter, burn_cap)
    if operation_hash = system_operation_hash(self.contract_call_cmd(contract_address, parameter, burn_cap))
      { 'operation_hash' => operation_hash,
        'receipt' => self.get_receipt(operation_hash) }
    end
  end

  def get_storage_cmd(contract_address)
    "#{self.tezos_client_path} get contract storage for  #{contract_address}"
  end

  def get_storage(contract_address)
    system_lines(self.get_storage_cmd(contract_address))
  end
end

class LorentzContractParam
 @originated_contracts = {}
 @ran_calls = {}
 @users = {}

 class << self
   attr_accessor :originated_contracts, :ran_calls, :users
 end

 attr_accessor :config, :command, :storage_command, :storage_params, :burn_cap

 def path
   self.config['param_path']
 end

 def stack_path
   self.config['stack_path']
 end

 def stack_exec
   if self.stack_path.nil?
     ""
   else
     "#{self.stack_path} exec -- "
   end
 end

 def initialize(config, command, storage_command, storage_params, burn_cap)
   @config = config
   @command = command
   @storage_command = storage_command
   @storage_params = storage_params
   @burn_cap = burn_cap
 end

 def self.originated_contracts_path(file_path)
   File.basename(file_path, '.yaml') + ".originated.yaml"
 end

 def self.save_originated_contracts(file_path, yaml_configs)
   File.write(self.originated_contracts_path(file_path), YAML.dump({'config' => yaml_configs, 'originated' => self.originated_contracts, 'ran' => self.ran_calls}))
 end

 def self.init_originated_contracts(file_path, yaml_configs)
   originated_contracts_path = self.originated_contracts_path(file_path)
   self.originated_contracts = if File.exist?(originated_contracts_path)
     originated_yaml = YAML.load(File.read(originated_contracts_path))
     originated_configs = originated_yaml['config']
     unless originated_configs == yaml_configs
       raise "expected 'config' fields in #{file_path} and #{originated_contracts_path} to match"
     end
     self.ran_calls = originated_yaml['ran']
     originated_yaml['originated']
   else
     self.save_originated_contracts file_path, yaml_configs
     {}
   end
 end

 def self.find_originated_contract(contract_alias, origination)
   if self.originated_contracts.has_key?(contract_alias) &&
      !self.originated_contracts[contract_alias].nil? &&
      !self.originated_contracts[contract_alias]&.[]('address').nil?
     puts "#{contract_alias} already originated: #{self.originated_contracts[contract_alias]}"
     existing_origination = self.originated_contracts[contract_alias]
     unless origination['config'] == existing_origination['config']
       raise "#{contract_alias} originated with:\n#{existing_origination} \nbut found #{origination}"
     end
     existing_origination
   end
 end

 def self.find_ran_call(contract_call_alias, ran_call)
   if self.ran_calls.has_key?(contract_call_alias) &&
      !self.ran_calls[contract_call_alias].nil? &&
      !self.ran_calls[contract_call_alias]&.[]('operation_hash').nil?
     puts "#{contract_call_alias} already ran: #{self.ran_calls[contract_call_alias]}"
     existing_ran_call = self.ran_calls[contract_call_alias]
     unless ran_call['config'] == existing_ran_call['config']
       raise "#{contract_call_alias} ran with:\n#{existing_ran_call} \nbut found #{ran_call}"
     end
     existing_ran_call
   end
 end


 def self.all_names()
   self.originated_contracts.to_a.map do |k,originated_contract|
     [k, originated_contract['address']]
   end.to_h.merge(self.users) do |user,x,y|
     if x == y
       x
     else
       raise "redefined #{user} as a user"
     end
   end
 end

 def self.from_yaml(file_path)
   yaml = YAML.load(File.read(file_path))
   yaml_configs = yaml['config']

   yaml['do'].each do |action|
     if action.is_a?(Hash) && action.has_key?('user')
       new_user = action.delete 'user'
       new_user_name = new_user.delete 'name'
       new_user_address = new_user.delete 'address'
       if self.users.has_key?(new_user_name)
         unless self.users[new_user_name] == new_user_address
           raise "user #{new_user_name} has two addresses: #{self.users[new_user_name]} #{new_user_address}"
         end
       else
         self.users[new_user_name] = new_user_address
       end
     elsif action.is_a?(Hash) && action.has_key?('originate')
       origination = action.delete 'originate'
       unless origination.has_key?('alias')
         raise "origination must have 'alias' key"
       end
       contract_alias = origination['alias']

       self.init_originated_contracts file_path, yaml_configs
       if !self.find_originated_contract(contract_alias, origination).nil?
       else
         origination_config = origination['config']
         aliased_origination_config = origination_config
         if origination_config.match(/^\$/)
           aliased_origination_config = yaml_configs[origination_config[1..-1]]
         end
         tezos_client = TezosClient.new(aliased_origination_config['tezos_client_path'], aliased_origination_config['user_address'])
         lorentz_contract =
           LorentzContractParam.new(aliased_origination_config,
                                    origination['command'],
                                    origination['storage_command'],
                                    origination['initial_storage'],
                                    origination['burn_cap'])
         result_address =
           lorentz_contract.originate(tezos_client,
                                      contract_alias,
                                      self.all_names)
         origination['address'] = result_address
         self.originated_contracts[contract_alias] = { 'address' => result_address, 'config' => origination_config }

         # Commit ASAP to prevent the need to re-originate if some part fails
         self.save_originated_contracts file_path, yaml_configs
       end
     elsif action.is_a?(Hash) && action.has_key?('run')
       contract_call = action.delete 'run'
       unless contract_call.has_key?('alias')
         raise "contract_call must have 'alias' key"
       end
       contract_call_alias = contract_call['alias']

       if !self.find_ran_call(contract_call_alias, contract_call).nil?
       else
         contract_call_config = contract_call['config']
         aliased_contract_call_config = contract_call_config
         if contract_call_config.match(/^\$/)
           aliased_contract_call_config = yaml_configs[contract_call_config[1..-1]]
         end
         tezos_client = TezosClient.new(aliased_contract_call_config['tezos_client_path'], aliased_contract_call_config['user_address'])
         lorentz_contract_call =
           LorentzContractCall.new(aliased_contract_call_config,
                                   contract_call['contract'],
                                   contract_call['command'],
                                   contract_call['parameters'],
                                   contract_call['burn_cap'])

         contract_call_result =
           lorentz_contract_call.contract_call(tezos_client,
                                               contract_alias)
         result_operation_hash = contract_call_result['operation_hash']
         self.ran_calls[contract_call_alias] =
           { 'contract' => contract_call['contract'],
             'parameters' => contract_call['parameters'],
             'operation_hash' => result_operation_hash,
             'config' => contract_call_config }

         # Commit ASAP to prevent the need to re-run if some part fails
         self.save_originated_contracts file_path, yaml_configs
       end
     else
       raise "unexpected action: #{action.inspect}"
     end
   end
 end

 def in_path(&block)
   Dir.chdir(self.path, &block)
 end

 def originate_cmd(tezos_client, contract_name, names)
   tezos_client.originate_cmd(contract_name,
                              self.print(),
                              self.storage(names),
                              @burn_cap)
 end

 def originate(tezos_client, contract_name, names)
   tezos_client.originate(contract_name,
                          self.print(),
                          self.storage(names),
                          @burn_cap)
 end

 def print()
   @contract_code || (@contract_code = self.in_path do
       system_line "#{self.stack_exec}#{@command} --oneline"
     end
   )
 end

 def storage(names)
   @storage_code || (@storage_code =
     storage_param_args = storage_params.to_a.map do |storage_param, storage_param_arg|
       if storage_param_arg.to_s.match(/^\$/)
         param_var = storage_param_arg.to_s[1..-1]
         if names.has_key?(param_var) && !names[param_var].nil?
           storage_param_arg = names[param_var]
         end
       end
       "--#{storage_param} #{storage_param_arg}"
     end.join(' ')

     self.in_path do
       system_line("#{self.stack_exec}#{@storage_command} " + storage_param_args)
     end
   )
 end
end


class LorentzContractCall < LorentzContractParam
  attr_accessor :config, :contract_address, :burn_cap

  def initialize(config, contract_address, command, parameters, burn_cap)
    @config = config
    @contract_address = contract_address
    @storage_command = command
    @storage_params = parameters
    @burn_cap = burn_cap
  end

  def contract_address
    if /^\$/ === @contract_address # === /^\$/
      found_address = LorentzContractParam.all_names[@contract_address[1..-1]]
      if !found_address.nil?
        @contract_address = found_address
      else
        @contract_address
      end
    else
      @contract_address
    end
  end

  def michelson_parameters
    LorentzContractParam.instance_method(:storage).bind(self).call(LorentzContractParam.all_names)
  end

  def contract_call_cmd(tezos_client, contract_call_alias)
    tezos_client.contract_call_cmd(self.contract_address,
                                   self.michelson_parameters,
                                   self.burn_cap)
  end

  def contract_call(tezos_client, contract_call_alias)
    tezos_client.contract_call(self.contract_address,
                               self.michelson_parameters,
                               self.burn_cap)
  end
end


if __FILE__ == $0
  if ARGV[1]&.strip == 'info'
    file_path = ARGV[0]
    originated_contracts_path = LorentzContractParam.originated_contracts_path(file_path)
    originated_yaml = YAML.load(File.read(originated_contracts_path))
    originated_yaml['originated'].each do |contract|
      p contract
      contract_address = contract[1]['address']
      base_config = originated_yaml['config']['base-config']
      tezos_client = TezosClient.new(base_config['tezos_client_path'], base_config['user_address'])
      tezos_client.get_storage(contract_address).to_a
    end
  end

  LorentzContractParam.from_yaml ARGV[0]
end

  # LorentzContractParam.from_yaml 'managed_ledger.yaml'
  #
  # # is run from cmd line
  # lorentz_contract_param_path, stack_path = ARGV[0], ARGV[1]
  # puts "lorentz_contract_param_path, stack_path = #{ARGV[0]}, #{ARGV[1]}"
  #
  # tezos_client_path, user_address = ARGV[2], ARGV[3]
  # puts "tezos_client_path, user_address = #{ARGV[2]}, #{ARGV[3]}"
  #
  # tezos_client = TezosClient.new tezos_client_path, user_address
  # admin_address = user_address
  # burn_cap = 100
  # managed_ledger =
  #   LorentzContractParam.new(lorentz_contract_param_path,
  #                            stack_path,
  #                            "ManagedLedger",
  #                            "ManagedLedgerBabylon",
  #                            {'admin' => admin_address},
  #                            burn_cap)
  #
  # # puts 'print'
  # # p managed_ledger.print
  # # puts 'storage'
  # # p managed_ledger.storage
  #
  # # ledger_originate_cmd = managed_ledger.originate_cmd(tezos_client)
  # # puts
  # # puts
  # # puts ledger_originate_cmd
  #
  # # print the originated contract hash
  # p managed_ledger.originate(tezos_client)

