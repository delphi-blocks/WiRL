{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
program WiRL.Tests.Framework;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}
uses
  FastMM4,
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  WiRL.Tests.Framework.Core in 'Source\WiRL.Tests.Framework.Core.pas',
  WiRL.Tests.Mock.Server in '..\Mock\WiRL.Tests.Mock.Server.pas',
  WiRL.Tests.Framework.Resources in 'Source\WiRL.Tests.Framework.Resources.pas',
  WiRL.Tests.Mock.Resources in '..\Mock\WiRL.Tests.Mock.Resources.pas',
  WiRL.Tests.Framework.Request in 'Source\WiRL.Tests.Framework.Request.pas',
  WiRL.Tests.Framework.Response in 'Source\WiRL.Tests.Framework.Response.pas',
  WiRL.Tests.Mock.Filters in '..\Mock\WiRL.Tests.Mock.Filters.pas',
  WiRL.Tests.Framework.Filters in 'Source\WiRL.Tests.Framework.Filters.pas',
  WiRL.Tests.Framework.Serialization in 'Source\WiRL.Tests.Framework.Serialization.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
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
