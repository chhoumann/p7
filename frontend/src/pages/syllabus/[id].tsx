import { NextPage } from 'next';
import { useRouter } from 'next/router'
import { prisma } from '../../server/db/client'
import React from 'react'


const SyllabusId: NextPage = async () => {

    const router = useRouter();
    const {id} = router.query;
    const sessions = await prisma.


    return (
        <div className='container flex justify-center items-center w-full h-[80vh]'>
            <div className='flex flex-col bg-gray-500 bg-opacity-75 mt-40 w-[50vh] h-full border-solid border-2 border-red-700'>
                <SessionRow name={"hello"} id={"Test"}/>
            </div>
            
        </div>
    )
}

//grid h-screen justify-center
// grid-cols-1 w-96 top-5
export default SyllabusId; 


function SessionRow({id, name}:{id: string, name: string}) {


    return (
        <div className='bg-red-400'>
            <span>This is true</span>
        </div>
    )
}