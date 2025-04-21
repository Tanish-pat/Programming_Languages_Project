import Link from "next/link"
import { Plus } from "lucide-react"

import { Button } from "@/components/ui/button"
import { DataTable } from "@/components/data-table"
import { orderColumns } from "@/components/order-columns"
import { getOrderData } from "@/lib/data"

export default async function OrdersPage() {
  const data = await getOrderData()

  return (
    <div className="flex-1 space-y-4 p-4 pt-6 md:p-8">
      <div className="flex flex-col items-start justify-between space-y-2 md:flex-row md:items-center md:space-y-0">
        <h2 className="text-3xl font-bold tracking-tight">Orders</h2>
        <Link href="/orders/add">
          <Button>
            <Plus className="mr-2 h-4 w-4" />
            Add Order
          </Button>
        </Link>
      </div>
      <div className="space-y-4">
        <DataTable columns={orderColumns} data={data} searchKey="customer" />
      </div>
    </div>
  )
}
