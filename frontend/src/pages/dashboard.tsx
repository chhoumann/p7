import { GetServerSideProps, NextPage } from "next";
import React, { useEffect, useState } from "react";
import Layout from "../components/layout";
import { getServerAuthSession } from "../server/common/get-server-auth-session";
import { trpc } from "../utils/trpc";
import {
    createColumnHelper,
    useReactTable,
    getCoreRowModel,
    flexRender,
} from "@tanstack/react-table";
import { Tab } from "@headlessui/react";
import clsx from "clsx";

interface ProblemSubmissionData {
    success: boolean;
    user: {
        name: string | null;
    };
}

const columnHelper = createColumnHelper<ProblemSubmissionData>();

const columns = [
    columnHelper.accessor("user.name", {
        id: "Student",
        header: "Student",
        cell: (info) => info.getValue(),
    }),
    columnHelper.accessor("success", {
        id: "Success",
        header: "Success",
        cell: (info) => (info.getValue() ? "✅" : "❌"),
    }),
];

const OverviewPage: NextPage = () => {
    const [selectedSyllabus, setSelectedSyllabus] = useState<string>("");
    const [didSetSyllabusAfterFirstLoad, setDidSetSyllabusAfterFirstLoad] =
        useState(false);

    const syllabi = trpc.useQuery(["syllabus.findAll"]);
    const { data, isSuccess } = trpc.useQuery(
        ["dashboard.dashboardData", selectedSyllabus],
        {
            enabled: !!selectedSyllabus,
        }
    );

    useEffect(() => {
        if (didSetSyllabusAfterFirstLoad) return;

        if (
            syllabi.isSuccess &&
            syllabi.data &&
            syllabi.data.length &&
            syllabi.data[0]?.name
        ) {
            setSelectedSyllabus(syllabi.data[0].name);
            setDidSetSyllabusAfterFirstLoad(true);
        }
    }, [syllabi, didSetSyllabusAfterFirstLoad]);

    return (
        <Layout title="Overview">
            {!syllabi.isSuccess ? (
                <div></div>
            ) : (
                <div className="container w-1/4 mx-auto">
                    <select
                        className="w-full p-2 rounded-lg"
                        name="syllabiSelector"
                        id="syllabiSelector"
                        onChange={(evt) =>
                            setSelectedSyllabus(evt.target.value)
                        }
                        value={selectedSyllabus}
                    >
                        <option value="">Select a syllabus</option>
                        {syllabi.data.map(({ name }) => (
                            <option value={name} key={name}>
                                {name}
                            </option>
                        ))}
                    </select>
                </div>
            )}
            {!isSuccess ? (
                <div></div>
            ) : (
                <div className="flex flex-col mt-10 h-screen w-2/3 mx-auto">
                    <Tab.Group>
                        <Tab.List className="flex space-x-1 rounded-xl bg-blue-900/20 p-1">
                            {data
                                ? data.map((problemSet) => (
                                      <Tab
                                          key={problemSet.topic}
                                          className={({ selected }) =>
                                              clsx(
                                                  "w-full rounded-lg py-2.5 text-sm font-medium leading-5 text-blue-700",
                                                  "ring-white ring-opacity-60 ring-offset-2 ring-offset-blue-400 focus:outline-none focus:ring-2",
                                                  selected
                                                      ? "bg-white shadow font-bold"
                                                      : "text-white hover:bg-white/[0.12] font-semibold"
                                              )
                                          }
                                      >
                                          {problemSet.topic}
                                      </Tab>
                                  ))
                                : null}
                        </Tab.List>

                        <Tab.Panels>
                            {data
                                ? data.map((problemSet) => (
                                      <Tab.Panel
                                          key={problemSet.topic}
                                          className={clsx(
                                              "rounded-xl bg-white p-3"
                                          )}
                                      >
                                          {problemSet.Problems.map(
                                              (problem) => {
                                                  const numSolved =
                                                      problem.Submission.filter(
                                                          (s) => s.success
                                                      ).length;
                                                  const totalSubmissions =
                                                      problem.Submission.length;

                                                  return (
                                                      <div
                                                          key={problem.name}
                                                          className="flex flex-col w-2/3 gap-4 mx-auto my-8"
                                                      >
                                                          <h1 className="text-3xl my-4">
                                                              {problem.name} -{" "}
                                                              {numSolved} /{" "}
                                                              {totalSubmissions}
                                                          </h1>
                                                          <div className="flex flex-col justify-center mx-auto w-full">
                                                              <ProblemTable
                                                                  data={
                                                                      problem.Submission
                                                                  }
                                                              />
                                                          </div>
                                                      </div>
                                                  );
                                              }
                                          )}
                                      </Tab.Panel>
                                  ))
                                : null}
                        </Tab.Panels>
                    </Tab.Group>
                </div>
            )}
        </Layout>
    );
};

export default OverviewPage;

function ProblemTable({ data }: { data: ProblemSubmissionData[] }) {
    const table = useReactTable({
        columns,
        data: data,
        getCoreRowModel: getCoreRowModel(),
    });
    return (
        <table>
            <thead className="my-4">
                {table.getHeaderGroups().map((headerGroup) => (
                    <tr key={headerGroup.id} className="border-b">
                        {headerGroup.headers.map((header) => (
                            <th key={header.id} className="text-left p-2">
                                {header.isPlaceholder
                                    ? null
                                    : flexRender(
                                          header.column.columnDef.header,
                                          header.getContext()
                                      )}
                            </th>
                        ))}
                    </tr>
                ))}
            </thead>
            <tbody>
                {table.getRowModel().rows.map((row) => (
                    <tr key={row.id}>
                        {row.getVisibleCells().map((cell) => (
                            <td key={cell.id} className="p-2">
                                {flexRender(
                                    cell.column.columnDef.cell,
                                    cell.getContext()
                                )}
                            </td>
                        ))}
                    </tr>
                ))}
            </tbody>
            <tfoot>
                {table.getFooterGroups().map((footerGroup) => (
                    <tr key={footerGroup.id}>
                        {footerGroup.headers.map((header) => (
                            <th key={header.id}>
                                {header.isPlaceholder
                                    ? null
                                    : flexRender(
                                          header.column.columnDef.footer,
                                          header.getContext()
                                      )}
                            </th>
                        ))}
                    </tr>
                ))}
            </tfoot>
        </table>
    );
}

export const getServerSideProps: GetServerSideProps = async (ctx) => {
    const session = await getServerAuthSession(ctx);

    if (!session || !session.user || session.user.role !== "teacher") {
        return {
            redirect: {
                destination: "/",
                permanent: false,
            },
        };
    }

    return {
        props: {},
    };
};
