import { createRouter } from "./context";
import { z } from "zod";
import * as trpc from "@trpc/server";
import { prisma } from "../db/client";
import {ProblemSet} from "@prisma/client";

const zodObjProblemSet = z.object({
  topic: z.string(),
  date: z.date(),
  syllabusId: z.string(),
})


export const problemSetsRouter = createRouter().query("getBySyllabusId", {
  input: z.string(),
  async resolve({ input: id }) {
    console.log("id is: ", id)
    const problemSets: ProblemSet[] | null = await prisma.problemSet.findMany({
      where: { syllabusId: id },
    });

    if (!problemSets || problemSets.length === 0) {
      throw new trpc.TRPCError({
        code: "NOT_FOUND",
        message: `ProblemSets with syllabusId ${id} not found`,
      });
    }

    return { problemSets };
  },
}).mutation("postProblemSet", {
  input: zodObjProblemSet,
  async resolve({input}) {
    try {
      await prisma.problemSet.create({data: input})
      return {success: true, result: "Successfully created problem!"}
    } catch (error) {
      throw new trpc.TRPCError({
        code: "INTERNAL_SERVER_ERROR",
        message: "Could not post problemset to database.",
        cause: error
      })
    }
  }
});
