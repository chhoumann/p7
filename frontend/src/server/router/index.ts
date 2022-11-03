// src/server/router/index.ts
import { createRouter } from "./context";
import superjson from "superjson";

import { exampleRouter } from "./example";
import { protectedExampleRouter } from "./protected-example-router";
import { codeRouter } from "./code";
import { syllabusRouter } from "./syllabusRouter";
import { problemRouter } from "./problemRouter";
import { problemSetsRouter } from "./problemSetRouter";
import { TestRouter } from "./testRouter";

export const appRouter = createRouter()
    .transformer(superjson)
    .merge("code.", codeRouter)
    .merge("example.", exampleRouter)
    .merge("auth.", protectedExampleRouter)
    .merge("syllabus.", syllabusRouter)
    .merge("problem.", problemRouter)
    .merge("problemSets.", problemSetsRouter)
    .merge("test.", TestRouter);

// export type definition of API
export type AppRouter = typeof appRouter;
