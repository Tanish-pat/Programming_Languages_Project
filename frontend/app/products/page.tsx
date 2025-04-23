"use client"

import type React from "react"

import { useState, useEffect } from "react"
import { useRouter } from "next/navigation"
import { Plus, MoreHorizontal } from "lucide-react"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { DataTable } from "@/components/ui/data-table"
import { Badge } from "@/components/ui/badge"
import { ProductAPI } from "@/lib/api"
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
import { Textarea } from "@/components/ui/textarea"
import { DropdownMenu, DropdownMenuContent, DropdownMenuItem, DropdownMenuTrigger } from "@/components/ui/dropdown-menu"
import type { ColumnDef } from "@tanstack/react-table"

interface Product {
  sku: string
  name: string
  description: string
  price: number
  tags: string
}

export default function ProductsPage() {
  const router = useRouter()
  const { toast } = useToast()
  const [products, setProducts] = useState<Product[]>([])
  const [isLoading, setIsLoading] = useState(true)
  const [isDialogOpen, setIsDialogOpen] = useState(false)
  const [isDeleteDialogOpen, setIsDeleteDialogOpen] = useState(false)
  const [currentProduct, setCurrentProduct] = useState<Product | null>(null)

  useEffect(() => {
    fetchProducts()
  }, [])

  const fetchProducts = async () => {
    setIsLoading(true)
    try {
      const data = await ProductAPI.getAll()
      setProducts(data)
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to fetch products",
        variant: "destructive",
      })
    } finally {
      setIsLoading(false)
    }
  }

  const handleAddNew = () => {
    setCurrentProduct({
      sku: "",
      name: "",
      description: "",
      price: 0,
      tags: "",
    })
    setIsDialogOpen(true)
  }

  const handleEdit = (product: Product) => {
    setCurrentProduct(product)
    setIsDialogOpen(true)
  }

  const handleDelete = (product: Product) => {
    setCurrentProduct(product)
    setIsDeleteDialogOpen(true)
  }

  const confirmDelete = async () => {
    if (!currentProduct) return

    try {
      await ProductAPI.delete(currentProduct.sku)
      toast({
        title: "Success",
        description: "Product deleted successfully",
      })
      fetchProducts()
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to delete product",
        variant: "destructive",
      })
    } finally {
      setIsDeleteDialogOpen(false)
    }
  }

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
    const { name, value, type } = e.target as HTMLInputElement

    if (!currentProduct) return

    setCurrentProduct({
      ...currentProduct,
      [name]: type === "number" ? Number(value) : value,
    })
  }

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!currentProduct) return

    try {
      // Check if product exists by SKU
      const isNewProduct = !products.some((p) => p.sku === currentProduct.sku)

      if (isNewProduct) {
        await ProductAPI.create(currentProduct)
        toast({
          title: "Success",
          description: "Product created successfully",
        })
      } else {
        await ProductAPI.update(currentProduct.sku, currentProduct)
        toast({
          title: "Success",
          description: "Product updated successfully",
        })
      }
      setIsDialogOpen(false)
      fetchProducts()
    } catch (error) {
      toast({
        title: "Error",
        description: `Failed to ${!products.some((p) => p.sku === currentProduct.sku) ? "create" : "update"} product`,
        variant: "destructive",
      })
    }
  }

  const columns: ColumnDef<Product>[] = [
    {
      accessorKey: "sku",
      header: "SKU",
    },
    {
      accessorKey: "name",
      header: "Name",
    },
    {
      accessorKey: "description",
      header: "Description",
      cell: ({ row }) => {
        const description = row.original.description
        return description.length > 50 ? `${description.substring(0, 50)}...` : description
      },
    },
    {
      accessorKey: "price",
      header: "Price",
      cell: ({ row }) => `$${row.original.price.toFixed(2)}`,
    },
    {
      accessorKey: "tags",
      header: "Tags",
      cell: ({ row }) => (
        <div className="flex flex-wrap gap-1">
          {row.original.tags.split(",").map((tag, index) => (
            <Badge key={index} variant="outline" className="mr-1">
              {tag.trim()}
            </Badge>
          ))}
        </div>
      ),
    },
    {
      id: "actions",
      cell: ({ row }) => (
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button variant="ghost" size="icon">
              <MoreHorizontal className="h-4 w-4" />
              <span className="sr-only">Open menu</span>
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end">
            <DropdownMenuItem onClick={() => router.push(`/products/${row.original.sku}`)}>
              View Details
            </DropdownMenuItem>
            <DropdownMenuItem onClick={() => handleEdit(row.original)}>Edit</DropdownMenuItem>
            <DropdownMenuItem onClick={() => handleDelete(row.original)}>Delete</DropdownMenuItem>
          </DropdownMenuContent>
        </DropdownMenu>
      ),
    },
  ]

  return (
    <div className="space-y-4">
      <div className="flex justify-between items-center">
        <h1 className="text-2xl font-bold tracking-tight">Products</h1>
        <Button onClick={handleAddNew}>
          <Plus className="mr-2 h-4 w-4" /> Add Product
        </Button>
      </div>

      <Card>
        <CardHeader>
          <CardTitle>Product Management</CardTitle>
          <CardDescription>View and manage your product catalog</CardDescription>
        </CardHeader>
        <CardContent>
          {isLoading ? (
            <div className="flex justify-center items-center h-24">Loading products...</div>
          ) : (
            <DataTable columns={columns} data={products} searchColumn="name" searchPlaceholder="Search products..." />
          )}
        </CardContent>
      </Card>

      <Dialog open={isDialogOpen} onOpenChange={setIsDialogOpen}>
        <DialogContent>
          <form onSubmit={handleSubmit}>
            <DialogHeader>
              <DialogTitle>
                {!products.some((p) => p.sku === currentProduct?.sku) ? "Add New Product" : "Edit Product"}
              </DialogTitle>
              <DialogDescription>
                {!products.some((p) => p.sku === currentProduct?.sku)
                  ? "Add a new product to your catalog"
                  : "Edit the product information"}
              </DialogDescription>
            </DialogHeader>
            <div className="grid gap-4 py-4">
              <div className="space-y-2">
                <Label htmlFor="sku">SKU</Label>
                <Input
                  id="sku"
                  name="sku"
                  value={currentProduct?.sku}
                  onChange={handleInputChange}
                  required
                  disabled={products.some((p) => p.sku === currentProduct?.sku)}
                />
              </div>
              <div className="space-y-2">
                <Label htmlFor="name">Name</Label>
                <Input id="name" name="name" value={currentProduct?.name} onChange={handleInputChange} required />
              </div>
              <div className="space-y-2">
                <Label htmlFor="description">Description</Label>
                <Textarea
                  id="description"
                  name="description"
                  value={currentProduct?.description}
                  onChange={handleInputChange}
                  rows={3}
                />
              </div>
              <div className="space-y-2">
                <Label htmlFor="price">Price</Label>
                <Input
                  id="price"
                  name="price"
                  type="number"
                  min="0"
                  step="0.01"
                  value={currentProduct?.price}
                  onChange={handleInputChange}
                  required
                />
              </div>
              <div className="space-y-2">
                <Label htmlFor="tags">Tags (comma separated)</Label>
                <Input id="tags" name="tags" value={currentProduct?.tags} onChange={handleInputChange} />
              </div>
            </div>
            <DialogFooter>
              <Button type="submit">Save</Button>
            </DialogFooter>
          </form>
        </DialogContent>
      </Dialog>

      <Dialog open={isDeleteDialogOpen} onOpenChange={setIsDeleteDialogOpen}>
        <DialogContent>
          <DialogHeader>
            <DialogTitle>Confirm Deletion</DialogTitle>
            <DialogDescription>
              Are you sure you want to delete product "{currentProduct?.name}"? This action cannot be undone.
            </DialogDescription>
          </DialogHeader>
          <DialogFooter>
            <Button variant="outline" onClick={() => setIsDeleteDialogOpen(false)}>
              Cancel
            </Button>
            <Button variant="destructive" onClick={confirmDelete}>
              Delete
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
    </div>
  )
}
