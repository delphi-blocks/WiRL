{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2024 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
program WiRL.Tests.Framework;

{$IFNDEF DEBUG}
{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$ENDIF}
{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  {$IFDEF DEBUG}
  DUnitX.Loggers.GUI.VCL,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  WiRL.Tests.Framework.Core in 'Source\WiRL.Tests.Framework.Core.pas',
  WiRL.Tests.Framework.Resources in 'Source\WiRL.Tests.Framework.Resources.pas',
  WiRL.Tests.Framework.Request in 'Source\WiRL.Tests.Framework.Request.pas',
  WiRL.Tests.Framework.Response in 'Source\WiRL.Tests.Framework.Response.pas',
  WiRL.Tests.Framework.Filters in 'Source\WiRL.Tests.Framework.Filters.pas',
  WiRL.Tests.Framework.Serialization in 'Source\WiRL.Tests.Framework.Serialization.pas',
  WiRL.Tests.Framework.HeaderParser in 'Source\WiRL.Tests.Framework.HeaderParser.pas',
  WiRL.Tests.Framework.ContentNegotiation in 'Source\WiRL.Tests.Framework.ContentNegotiation.pas',
  WiRL.Tests.Framework.ConvertRequest in 'Source\WiRL.Tests.Framework.ConvertRequest.pas',
  WiRL.Tests.Framework.ContextInjection in 'Source\WiRL.Tests.Framework.ContextInjection.pas',
  WiRL.Tests.Framework.Converter in 'Source\WiRL.Tests.Framework.Converter.pas',
  WiRL.Tests.Framework.Validators in 'Source\WiRL.Tests.Framework.Validators.pas',
  WiRL.Tests.Framework.MultiParts in 'Source\WiRL.Tests.Framework.MultiParts.pas',
  WiRL.Tests.Framework.MessageBody in 'Source\WiRL.Tests.Framework.MessageBody.pas',
  WiRL.Tests.Framework.ExceptionMapper in 'Source\WiRL.Tests.Framework.ExceptionMapper.pas',
  WiRL.Tests.Framework.GarbageCollector in 'Source\WiRL.Tests.Framework.GarbageCollector.pas',
  WiRL.Tests.Mock.Classes in '..\Mock\WiRL.Tests.Mock.Classes.pas',
  WiRL.Tests.Mock.Server in '..\Mock\WiRL.Tests.Mock.Server.pas',
  WiRL.Tests.Mock.Filters in '..\Mock\WiRL.Tests.Mock.Filters.pas',
  WiRL.Tests.Mock.Validators in '..\Mock\WiRL.Tests.Mock.Validators.pas',
  WiRL.Tests.Mock.Resources.Core in '..\Mock\WiRL.Tests.Mock.Resources.Core.pas',
  WiRL.Tests.Mock.ExceptionMapper in '..\Mock\WiRL.Tests.Mock.ExceptionMapper.pas',
  WiRL.Tests.Mock.InjectionFactories in '..\Mock\WiRL.Tests.Mock.InjectionFactories.pas',
  WiRL.Tests.Mock.MessageBody.XML in '..\Mock\WiRL.Tests.Mock.MessageBody.XML.pas',
  WiRL.Tests.Mock.MessageBody.PersonObject in '..\Mock\WiRL.Tests.Mock.MessageBody.PersonObject.pas',
  WiRL.Tests.Mock.MessageBody.PersonRecord in '..\Mock\WiRL.Tests.Mock.MessageBody.PersonRecord.pas',
  WiRL.Tests.Mock.Resources.Validators in '..\Mock\WiRL.Tests.Mock.Resources.Validators.pas',
  WiRL.Tests.Mock.Resources.MessageBody in '..\Mock\WiRL.Tests.Mock.Resources.MessageBody.pas',
  WiRL.Tests.Mock.Resources.Exception in '..\Mock\WiRL.Tests.Mock.Resources.Exception.pas',
  WiRL.Tests.Mock.Resources.Convert in '..\Mock\WiRL.Tests.Mock.Resources.Convert.pas',
  WiRL.Tests.Mock.Resources.ContextInjection in '..\Mock\WiRL.Tests.Mock.Resources.ContextInjection.pas',
  WiRL.Tests.Framework.URL in 'Source\WiRL.Tests.Framework.URL.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
  ReportMemoryLeaksOnShutdown := True;
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  Exit;
{$ENDIF}
{$IFDEF DEBUG}
  DUnitX.Loggers.GUI.VCL.Run;
  Exit;
{$ENDIF}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
