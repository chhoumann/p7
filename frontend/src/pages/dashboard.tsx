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

const columnHelper = createColumnHelper<{
    id: string;
    success: boolean;
    user: {
        name: string | null;
    },
    problem: {
        id: string;
        name: string;
    }
}>();

const columns = [
    columnHelper.accessor("user.name", {
        id: "Student",
        header: "Student",
        cell: (info) => info.getValue(),
    }),
    columnHelper.accessor("problem.name", {
        id: "Problem",
        header: "Problem",
        cell: (info) => info.getValue(),
    }),
    columnHelper.accessor("success", {
        id: "Success",
        header: "Success",
        cell: (info) => info.getValue() ? "✅" : "❌",
    }),
];

const OverviewPage: NextPage = () => {
    const { data, isSuccess } = trpc.useQuery(["dashboard.dashboardData"]);

    const testProblemData = data ? data[0]?.ProblemSets[0]?.Problems[0]?.Submission : [];

    const table = useReactTable({
        columns,
        data: testProblemData ?? [],
        getCoreRowModel: getCoreRowModel(),
    });

    if (!isSuccess) {
        return <div>Loading...</div>;
    }

    return (
        <Layout title="Overview">
            <div>
                <table>
                    <thead>
                        {table.getHeaderGroups().map((headerGroup) => (
                            <tr key={headerGroup.id}>
                                {headerGroup.headers.map((header) => (
                                    <th key={header.id}>
                                        {header.isPlaceholder
                                            ? null
                                            : flexRender(
                                                  header.column.columnDef
                                                      .header,
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
                                                  header.column.columnDef
                                                      .footer,
                                                  header.getContext()
                                              )}
                                    </th>
                                ))}
                            </tr>
                        ))}
                    </tfoot>
                </table>
            </div>
        </Layout>
    );
};

export default OverviewPage;

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
