unit Server.MessageBody.WebStencils;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils, System.Rtti,

  WiRL.Core.JSON,
  WiRL.Core.Classes,
  WiRL.Core.Attributes,
  WiRL.Core.Declarations,
  WiRL.http.Core,
  WiRL.http.Headers,
  WiRL.http.Request,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Context,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBody.Classes,
  WiRL.Core.Exceptions,

  Web.Stencils;

type
  [Produces(TMediaType.TEXT_HTML)]
  TWiRLStencilsWriter = class(TMessageBodyProvider)
  private
    [Context] FRequest: TWiRLRequest;
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream); override;
  end;

implementation

{ TWiRLStencilsWriter }

procedure TWiRLStencilsWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LStencilName: string;
  LStencil: TWebStencilsProcessor;
  LContent: string;
  LBuffer: TBytes;
begin
  inherited;
  LStencilName := FRequest.Headers['x-stencil'];
  if LStencilName = '' then
    LStencilName := Copy(AValue.AsObject.ClassName, 2, 100).ToLower + '.html';
  LStencil := TWebStencilsProcessor.Create(nil);
  try
    LStencil.InputFileName := TPath.Combine(ExtractFileDir(ParamStr(0)), 'www', LStencilName);
    LStencil.AddVar('value', AValue.AsObject, False);
    LContent := LStencil.Content;
    //LBuffer := AMediaType.GetDelphiEncoding.GetBytes(LContent);
    LBuffer := TEncoding.UTF8.GetBytes(LContent);
    AContentStream.WriteBuffer(LBuffer[0], Length(LBuffer));
  finally
    LStencil.Free;
  end;
end;

procedure RegisterMessageBodyClasses;
begin
  TMessageBodyWriterRegistry.Instance.RegisterWriter<TObject>(TWiRLStencilsWriter);
end;

initialization

RegisterMessageBodyClasses;

end.
