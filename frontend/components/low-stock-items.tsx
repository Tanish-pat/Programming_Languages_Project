"use client"

import { useEffect, useState } from "react"
import Link from "next/link"
import { AlertCircle } from "lucide-react"

import { Button } from "@/components/ui/button"
import { Skeleton } from "@/components/ui/skeleton"
import { fetchLowStockItems } from "@/lib/api"
import { Badge } from "@/components/ui/badge"

interface Item {
  id: number
  name: string
  quantity: number
  threshold: number
}

export function LowStockItems() {
  const [items, setItems] = useState<Item[]>([])
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    const getItems = async () => {
      try {
        // Fetch low stock items from the API
        const data = await fetchLowStockItems()
        setItems(data)
      } catch (error) {
        console.error("Failed to fetch low stock items:", error)
      } finally {
        setLoading(false)
      }
    }

    getItems()
  }, [])

  if (loading) {
    return (
      <div className="space-y-2">
        {Array(5)
          .fill(0)
          .map((_, i) => (
            <Skeleton key={i} className="h-16 w-full" />
          ))}
      </div>
    )
  }

  return (
    <div className="space-y-4">
      {items.length === 0 ? (
        <div className="flex flex-col items-center justify-center py-4 text-center">
          <div className="rounded-full bg-green-100 p-3 dark:bg-green-900/20">
            <AlertCircle className="h-6 w-6 text-green-600 dark:text-green-400" />
          </div>
          <h3 className="mt-4 text-sm font-medium">All items are well-stocked</h3>
          <p className="mt-1 text-sm text-muted-foreground">There are no items below their threshold levels.</p>
        </div>
      ) : (
        <div className="rounded-md border">
          <div className="grid grid-cols-3 gap-4 p-4 text-sm font-medium text-muted-foreground">
            <div>Name</div>
            <div>Quantity</div>
            <div className="text-right">Action</div>
          </div>
          <div className="divide-y">
            {items.map((item) => (
              <div key={item.id} className="grid grid-cols-3 gap-4 p-4 text-sm">
                <div className="font-medium">{item.name}</div>
                <div className="flex items-center">
                  <Badge variant="destructive" className="mr-2">
                    {item.quantity} left
                  </Badge>
                  <span className="text-xs text-muted-foreground">(Threshold: {item.threshold})</span>
                </div>
                <div className="text-right">
                  <Button size="sm" variant="outline" asChild>
                    <Link href={`/inventory/${item.id}/restock`}>Restock</Link>
                  </Button>
                </div>
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  )
}
