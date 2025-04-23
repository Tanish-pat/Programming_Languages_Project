"use client"

import type React from "react"

import { useState, useEffect } from "react"
import { AlertCircle } from "lucide-react"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { DataTable } from "@/components/ui/data-table"
import { Badge } from "@/components/ui/badge"
import { InventoryAPI, ProductAPI } from "@/lib/api"
import { useToast } from "@/components/ui/use-toast"
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog"
import { Input } from "@/components/ui/input"
import { Label } from "@/components/ui/label"
import type { ColumnDef } from "@tanstack/react-table"

interface Inventory {
  productId: string
  quantity: number
  location: string
  productName?: string
}

export default function InventoryPage() {
  const { toast } = useToast()
  const [inventory, setInventory] = useState<Inventory[]>([])
  const [isLoading, setIsLoading] = useState(true)
  const [isDialogOpen, setIsDialogOpen] = useState(false)
  const [currentItem, setCurrentItem] = useState<Inventory | null>(null)

  useEffect(() => {
    fetchInventory()
  }, [])

  const fetchInventory = async () => {
    setIsLoading(true)
    try {
      const data = await InventoryAPI.getAll()

      // Fetch product names for each inventory item
      const inventoryWithNames = await Promise.all(
        data.map(async (item) => {
          try {
            const product = await ProductAPI.getBySku(item.productId)
            return {
              ...item,
              productName: product.name,
            }
          } catch (error) {
            return {
              ...item,
              productName: "Unknown Product",
            }
          }
        }),
      )

      setInventory(inventoryWithNames)
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to fetch inventory",
        variant: "destructive",
      })
    } finally {
      setIsLoading(false)
    }
  }

  const handleEdit = (item: Inventory) => {
    setCurrentItem(item)
    setIsDialogOpen(true)
  }

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { name, value, type } = e.target

    if (!currentItem) return

    setCurrentItem({
      ...currentItem,
      [name]: type === "number" ? Number(value) : value,
    })
  }

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!currentItem) return

    try {
      await InventoryAPI.update(currentItem.productId, currentItem)
      toast({
        title: "Success",
        description: "Inventory updated successfully",
      })
      setIsDialogOpen(false)
      fetchInventory()
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to update inventory",
        variant: "destructive",
      })
    }
  }

  const getLowStockStatus = (quantity: number) => {
    if (quantity <= 0) return "Out of Stock"
    if (quantity < 10) return "Low Stock"
    return "In Stock"
  }

  const getStockBadgeVariant = (quantity: number) => {
    if (quantity <= 0) return "destructive"
    if (quantity < 10) return "warning"
    return "success"
  }

  const columns: ColumnDef<Inventory>[] = [
    {
      accessorKey: "productId",
      header: "Product ID",
    },
    {
      accessorKey: "productName",
      header: "Product Name",
    },
    {
      accessorKey: "quantity",
      header: "Quantity",
    },
    {
      accessorKey: "status",
      header: "Status",
      cell: ({ row }) => (
        <div className="flex items-center">
          <Badge variant={getStockBadgeVariant(row.original.quantity) as any}>
            {getLowStockStatus(row.original.quantity)}
          </Badge>
          {row.original.quantity < 10 && row.original.quantity > 0 && (
            <AlertCircle className="h-4 w-4 text-yellow-500 ml-2" />
          )}
        </div>
      ),
    },
    {
      accessorKey: "location",
      header: "Location",
    },
    {
      id: "actions",
      header: "Actions",
      cell: ({ row }) => (
        <Button variant="outline" size="sm" onClick={() => handleEdit(row.original)}>
          Update
        </Button>
      ),
    },
  ]

  return (
    <div className="space-y-4">
      <div className="flex justify-between items-center">
        <h1 className="text-2xl font-bold tracking-tight">Inventory</h1>
      </div>

      <Card>
        <CardHeader>
          <CardTitle>Inventory Management</CardTitle>
          <CardDescription>Track and manage your product inventory</CardDescription>
        </CardHeader>
        <CardContent>
          {isLoading ? (
            <div className="flex justify-center items-center h-24">Loading inventory...</div>
          ) : (
            <DataTable
              columns={columns}
              data={inventory}
              searchColumn="productName"
              searchPlaceholder="Search by product name..."
            />
          )}
        </CardContent>
      </Card>

      <Dialog open={isDialogOpen} onOpenChange={setIsDialogOpen}>
        <DialogContent>
          <form onSubmit={handleSubmit}>
            <DialogHeader>
              <DialogTitle>Update Inventory</DialogTitle>
              <DialogDescription>Update inventory quantity and location for this product</DialogDescription>
            </DialogHeader>
            <div className="grid gap-4 py-4">
              <div className="space-y-2">
                <Label>Product</Label>
                <Input value={currentItem?.productName} disabled />
              </div>
              <div className="space-y-2">
                <Label htmlFor="quantity">Quantity</Label>
                <Input
                  id="quantity"
                  name="quantity"
                  type="number"
                  min="0"
                  value={currentItem?.quantity}
                  onChange={handleInputChange}
                  required
                />
              </div>
              <div className="space-y-2">
                <Label htmlFor="location">Location</Label>
                <Input
                  id="location"
                  name="location"
                  value={currentItem?.location}
                  onChange={handleInputChange}
                  required
                />
              </div>
            </div>
            <DialogFooter>
              <Button type="submit">Update Inventory</Button>
            </DialogFooter>
          </form>
        </DialogContent>
      </Dialog>
    </div>
  )
}
