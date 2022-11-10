import { GetServerSideProps, NextPage } from "next";
import React from "react";
import Layout from "../components/layout";
import { getServerAuthSession } from "../server/common/get-server-auth-session";
import { trpc } from "../utils/trpc";
import {
    createColumnHelper,
    useReactTable,
    getCoreRowModel,
    flexRender,
} from "@tanstack/react-table";

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
    // columnHelper.accessor("problem.name", {
    //     id: "Problem",
    //     header: "Problem",
    //     cell: (info) => info.getValue(),
    // }),
    columnHelper.accessor("success", {
        id: "Success",
        header: "Success",
        cell: (info) => (info.getValue() ? "✅" : "❌"),
    }),
];

const OverviewPage: NextPage = () => {
    const { data, isSuccess } = trpc.useQuery(["dashboard.dashboardData"]);

    if (!isSuccess) {
        return <div>Loading...</div>;
    }

    return (
        <Layout title="Overview">
            <div className="flex flex-col mt-10 h-screen w-2/3 mx-auto gap-16">
                {data
                    ? data.map((problemSet) => (
                          <React.Fragment key={problemSet.topic}>
                              <h1 className="text-3xl">{problemSet.topic}</h1>
                              {problemSet.Problems.map((problem) => (
                                  <div
                                      key={problem.name}
                                      className="flex flex-col w-2/3 mx-auto"
                                  >
                                      <h1 className="text-2xl">
                                          {problem.name}
                                      </h1>
                                      <div className="flex flex-col justify-center w-2/3 mx-auto">
                                          <div className="flex flex-col justify-center w-2/3 mx-auto">
                                              <ProblemTable
                                                  data={problem.Submission}
                                              />
                                          </div>
                                      </div>
                                  </div>
                              ))}
                          </React.Fragment>
                      ))
                    : null}
            </div>
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
                            <th key={header.id}>
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
                            <td key={cell.id}>
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
