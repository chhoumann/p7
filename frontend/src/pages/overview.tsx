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

    if (!session || !session.user || session.user.role !== 'teacher') {
        return {
            redirect: {
                destination: '/',
                permanent: false,
            },
        };
    }

    return {
        props: { },
    };
};

