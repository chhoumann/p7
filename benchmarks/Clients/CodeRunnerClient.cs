﻿using System.Net.Http.Headers;
using System.Net.Http.Json;
using System.Text.Json;
using CodeRunnerClients.DataTransfer;

namespace CodeRunnerClients;

public class CodeRunnerClient
{
    protected readonly HttpClient _client = new();
    private static readonly Uri _postProblemUrl = CreateUri("haskell/submit");

    public CodeRunnerClient()
    {
        _client.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
        _client.DefaultRequestHeaders.Accept.Clear();
    }
    
    protected static Uri CreateUri(string endpointPath)
    {
        UriBuilder v = new()
        {
            Scheme = "http",
            Port = int.Parse(Environment.GetEnvironmentVariable("PORT") ?? throw new EnvironmentNotSetException("PORT")),
            Host = Environment.GetEnvironmentVariable("HOST") ?? throw new EnvironmentNotSetException("HOST"),
            Path = endpointPath
        };
        
        return v.Uri;
    }
    
    public Task<HttpResponseMessage> Post(CodeSubmission toSend)
    {
        return _client.PostAsJsonAsync(_postProblemUrl, toSend);
    }
    
    public async Task<PollIdResponse?> PostCodeRequest(string code, string test)
    {
        CodeSubmission toSend = new(code, test);
        return await JsonSerializer.DeserializeAsync<PollIdResponse>(await Post(toSend).Result.Content.ReadAsStreamAsync());
    }

}