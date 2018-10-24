object MicroCoinData: TMicroCoinData
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 271
  Width = 438
  object KeysConnection: TZConnection
    ControlsCodePage = cCP_UTF16
    Catalog = ''
    HostName = ''
    Port = 0
    Database = 'C:\Users\peter.nemeth\Documents\microcoin.keys'
    User = ''
    Password = ''
    Protocol = 'sqlite-3'
    Left = 46
    Top = 30
  end
  object KeysTable: TZTable
    Connection = KeysConnection
    UpdateObject = ZUpdateSQL1
    TableName = 'keys'
    UpdateMode = umUpdateAll
    Left = 44
    Top = 92
    object KeysTableid: TLargeintField
      AutoGenerateValue = arAutoInc
      FieldName = 'id'
      ReadOnly = True
    end
    object KeysTablekey_type: TIntegerField
      DisplayLabel = 'Curve'
      FieldName = 'key_type'
    end
    object KeysTablekey_name: TWideStringField
      DisplayLabel = 'Name'
      FieldName = 'key_name'
      Size = 255
    end
    object KeysTablex: TWideStringField
      FieldName = 'x'
      Visible = False
      Size = 255
    end
    object KeysTabley: TWideStringField
      FieldName = 'y'
      Visible = False
      Size = 255
    end
    object KeysTabled: TWideStringField
      FieldName = 'd'
      Visible = False
      Size = 255
    end
    object KeysTableencryption_mode: TIntegerField
      FieldName = 'encryption_mode'
      Visible = False
    end
  end
  object ZUpdateSQL1: TZUpdateSQL
    DeleteSQL.Strings = (
      'DELETE FROM keys'
      'WHERE'
      '  keys.id = :OLD_id')
    InsertSQL.Strings = (
      'INSERT INTO keys'
      '  (key_type, key_name, x, y, d, encryption_mode)'
      'VALUES'
      '  (:key_type, :key_name, :x, :y, :d, :encryption_mode)')
    ModifySQL.Strings = (
      'UPDATE keys SET'
      '  key_type = :key_type,'
      '  key_name = :key_name,'
      '  x = :x,'
      '  y = :y,'
      '  d = :d,'
      '  encryption_mode = :encryption_mode'
      'WHERE'
      '  keys.id = :OLD_id')
    UseSequenceFieldForRefreshSQL = False
    Left = 40
    Top = 156
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'key_type'
        ParamType = ptUnknown
      end
      item
        DataType = ftUnknown
        Name = 'key_name'
        ParamType = ptUnknown
      end
      item
        DataType = ftUnknown
        Name = 'x'
        ParamType = ptUnknown
      end
      item
        DataType = ftUnknown
        Name = 'y'
        ParamType = ptUnknown
      end
      item
        DataType = ftUnknown
        Name = 'd'
        ParamType = ptUnknown
      end
      item
        DataType = ftUnknown
        Name = 'encryption_mode'
        ParamType = ptUnknown
      end
      item
        DataType = ftUnknown
        Name = 'OLD_id'
        ParamType = ptUnknown
      end>
  end
end
