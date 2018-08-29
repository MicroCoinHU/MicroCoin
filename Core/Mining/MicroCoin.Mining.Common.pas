unit MicroCoin.Mining.Common;

interface

uses UCrypto;

type
  TMinerValuesForWork = Record
     block : Cardinal;
     version : Word;
     part1 : TRawBytes;
     payload_start : TRawBytes;
     part3 : TRawBytes;
     target : Cardinal;
     timestamp : Cardinal;
     target_pow : TRawBytes;
     // Stratum jobid
     jobid : String;
  End;


const

  CT_PoolMining_Method_STATUS = 'status';
  CT_PoolMining_Method_MINER_NOTIFY = 'miner-notify'; // Server message to clients to update miners PoW data
  CT_PoolMining_Method_MINER_SUBMIT = 'miner-submit'; // Client message to server to notify a PoW found
  CT_PoolMining_Method_STRATUM_MINING_AUTHORIZE = 'mining-authorize';
  CT_PoolMining_Method_STRATUM_MINING_SUBSCRIBE = 'mining-subscribe';

  CT_TMinerValuesForWork_NULL : TMinerValuesForWork = (block:0;version:0;part1:'';payload_start:'';part3:'';target:0;timestamp:0;target_pow:'';jobid:'');

implementation

end.
