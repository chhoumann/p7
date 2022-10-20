// src/server/router/index.ts
import { createRouter } from "./context";
import superjson from "superjson";

import { exampleRouter } from "./example";
import { protectedExampleRouter } from "./protected-example-router";
import { codeRouter } from "./code";
import { syllabusRouter } from "./syllabusRouter";

export const appRouter = createRouter()
  .transformer(superjson)
  .merge("code.", codeRouter)
  .merge("example.", exampleRouter)
  .merge("auth.", protectedExampleRouter)
  .merge("syllabus.", syllabusRouter);

// export type definition of API
export type AppRouter = typeof appRouter;
