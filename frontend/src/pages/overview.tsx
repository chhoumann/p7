import { PrismaClient } from "@prisma/client";
import { GetServerSideProps, NextPage } from "next";
import React from "react";
import Layout from "../components/layout";
import { getServerAuthSession } from "../server/common/get-server-auth-session";

const OverviewPage: NextPage = () => {
    return <Layout title="Overview">
        <div>OverviewPage</div>;
    </Layout>
};

export default OverviewPage;

export const getServerSideProps: GetServerSideProps = async (ctx) => {
    const session = await getServerAuthSession(ctx);

    const redir = {
        redirect: {
            destination: "/",
            permanent: false,
        },
    };

    if (!session?.user?.id) {
        return redir;
    }

    const db = prisma ?? new PrismaClient();

    const userRole = await db.user.findUnique({
        where: {
            id: session?.user.id,
        },
        select: {
            role: true,
        },
    });

    if (!userRole || userRole.role.name === "student") {
        return redir;
    }

    return {
        props: {
            s: session,
        },
    };
};
