// src/server/router/index.ts
import { createRouter } from "./context";
import superjson from "superjson";

import { codeRouter } from "./code";
import { syllabusRouter } from "./syllabusRouter";
import { problemRouter } from "./problemRouter";
import { problemSetsRouter } from "./problemSetRouter";
import { TestRouter } from "./testRouter";
import { submissionRouter } from "./submissionRouter";
import { dashboardRouter } from "./dashboardRouter";

export const appRouter = createRouter()
    .transformer(superjson)
    .merge("code.", codeRouter)
    .merge("syllabus.", syllabusRouter)
    .merge("problem.", problemRouter)
    .merge("problemSets.", problemSetsRouter)
    .merge("test.", TestRouter)
    .merge("submission.", submissionRouter)
    .merge("dashboard.", dashboardRouter);

// export type definition of API
export type AppRouter = typeof appRouter;
