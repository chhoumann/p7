using dotenv.net;

namespace CoderunnerClients;

internal static class EnvLoader
{
    private static bool _envLoaded;
    
    public static void Load()
    {
        if (! _envLoaded)
        {
            DotEnv.Fluent()
                .WithExceptions()
                .WithEnvFiles()
                .Load();
            _envLoaded = true;
        }
    }
}