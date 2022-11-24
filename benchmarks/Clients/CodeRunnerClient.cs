using System.Net.Http.Headers;
using System.Net.Http.Json;
using System.Text.Json;
using Client.DataTransfer;

namespace CodeRunnerClients;

public class CodeRunnerClient
{
    protected readonly HttpClient _client = new();
    protected static readonly Uri _postProblemUrl = CreateUri("haskell/submit");

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
    
    public async Task<PullIdResponse?> PostCodeRequest(string code, string test)
    {
        CodeSubmission toSend = new(code, test);
        return await JsonSerializer.DeserializeAsync<PullIdResponse>(await Post(toSend).Result.Content.ReadAsStreamAsync());
    }

}