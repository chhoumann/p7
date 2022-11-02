import { createRouter } from "./context";
import { z } from "zod";
import * as trpc from "@trpc/server";
import { prisma } from "../db/client";
import {Problem, ProblemSet} from "@prisma/client";
import ky from "ky";
import {env} from "../../env/server.mjs";

const postInput = z.object({
  id: z.string(),
  topic: z.string(),
  date: z.date(),
  syllabusId: z.string(),
})

const postOutput = z.object({
  success: z.boolean(),
  result: z.string()
});

export const problemSetsRouter = createRouter().query("getBySyllabusId", {
  input: z.string(),
  async resolve({ input: id }) {
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
  input: postInput,
  output: postOutput,
  async resolve({input}) {
    try {
      const webserverResponse = await ky
          .post(`${env.WEBSERVER_ADDRESS}/problemset`, {
            json: {
              ...input
            }
          }).json();

      const parsedResponse = z
          .object({
            result: z.string(),
            success: z.boolean(),
          })
          .safeParse(webserverResponse);

      if (!parsedResponse.success) {
        return {success: false, result: parsedResponse.error.message};
      }
      return parsedResponse.data;

    } catch (error) {
      throw new trpc.TRPCError({
        code: "INTERNAL_SERVER_ERROR",
        message: "Could not post problemset to database.",
        cause: error
      })
    }
  }
});
