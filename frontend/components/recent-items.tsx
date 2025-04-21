"use client"

import { useEffect, useState } from "react"
import Link from "next/link"
import { Edit, MoreHorizontal, Trash } from "lucide-react"

import { Button } from "@/components/ui/button"
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu"
import { Skeleton } from "@/components/ui/skeleton"
import { fetchRecentItems } from "@/lib/api"
import { formatCurrency } from "@/lib/utils"

interface Item {
  id: number
  name: string
  quantity: number
  price: number
  category: string
  createdAt: string
}

export function RecentItems() {
  const [items, setItems] = useState<Item[]>([])
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    const getItems = async () => {
      try {
        // Fetch recent inventory items from the API
        const data = await fetchRecentItems()
        setItems(data)
      } catch (error) {
        console.error("Failed to fetch recent items:", error)
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
        <p className="text-center text-muted-foreground">No items found</p>
      ) : (
        <div className="rounded-md border">
          <div className="grid grid-cols-5 gap-4 p-4 text-sm font-medium text-muted-foreground">
            <div>Name</div>
            <div>Category</div>
            <div>Quantity</div>
            <div>Price</div>
            <div className="text-right">Actions</div>
          </div>
          <div className="divide-y">
            {items.map((item) => (
              <div key={item.id} className="grid grid-cols-5 gap-4 p-4 text-sm">
                <div className="font-medium">{item.name}</div>
                <div>{item.category}</div>
                <div>{item.quantity}</div>
                <div>{formatCurrency(item.price)}</div>
                <div className="flex justify-end">
                  <DropdownMenu>
                    <DropdownMenuTrigger asChild>
                      <Button variant="ghost" size="icon">
                        <MoreHorizontal className="h-4 w-4" />
                        <span className="sr-only">Actions</span>
                      </Button>
                    </DropdownMenuTrigger>
                    <DropdownMenuContent align="end">
                      <DropdownMenuLabel>Actions</DropdownMenuLabel>
                      <DropdownMenuSeparator />
                      <DropdownMenuItem asChild>
                        <Link href={`/inventory/${item.id}`}>View details</Link>
                      </DropdownMenuItem>
                      <DropdownMenuItem asChild>
                        <Link href={`/inventory/${item.id}/edit`}>
                          <Edit className="mr-2 h-4 w-4" /> Edit
                        </Link>
                      </DropdownMenuItem>
                      <DropdownMenuItem className="text-destructive">
                        <Trash className="mr-2 h-4 w-4" /> Delete
                      </DropdownMenuItem>
                    </DropdownMenuContent>
                  </DropdownMenu>
                </div>
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  )
}
