namespace CodeRunnerClients;

public static class TaskBuilder
{
    public static IEnumerable<Task> BuildClientTaskList<T>(int count, Action<T> action)
        where T : CodeRunnerClient, new()
    {
        List<Action> clientActions = new(count);
        List<Task> tasks = new(count);
        
        for (int i = 0; i < count; i++)
        {
            T client = new();
            clientActions.Add(() => action.Invoke(client));
        }

        foreach (Action clientAction in clientActions)
        {
            tasks.Add(Task.Run(clientAction));
        }
        return tasks;
    }
}