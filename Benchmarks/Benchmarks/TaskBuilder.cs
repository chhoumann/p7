namespace Benchmarks;

public static class TaskBuilder
{
    public static IEnumerable<Task> BuildTaskList(int count, Action<CodeRunnerQueueClient> action)
    {
        List<Action> clientActions = new(count);
        List<Task> tasks = new(count);
        
        for (int i = 0; i < count; i++)
        {
            CodeRunnerQueueClient client = new();
            clientActions.Add(() => action.Invoke(client));
        }
        
        foreach (Action clientAction in clientActions)
        {
            tasks.Add(Task.Run(clientAction));
        }

        return tasks;
    }
}