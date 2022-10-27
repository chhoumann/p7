import { createRouter } from "./context";
import { z } from "zod";
import * as trpc from "@trpc/server";
import { prisma } from "../db/client";
import { ProblemSet } from "@prisma/client";

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
});
