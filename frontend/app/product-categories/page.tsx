"use client"

import type React from "react"

import { useState, useEffect } from "react"
import { Plus, Trash, MoreHorizontal } from "lucide-react"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { DataTable } from "@/components/ui/data-table"
import { ProductCategoryAPI, ProductAPI, CategoryAPI } from "@/lib/api"
import { useToast } from "@/components/ui/use-toast"
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog"
import { Label } from "@/components/ui/label"
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select"
import { DropdownMenu, DropdownMenuContent, DropdownMenuItem, DropdownMenuTrigger } from "@/components/ui/dropdown-menu"
import type { ColumnDef } from "@tanstack/react-table"

interface ProductCategory {
  productId: string
  categoryId: number
  productName?: string
  categoryName?: string
}

export default function ProductCategoriesPage() {
  const { toast } = useToast()
  const [productCategories, setProductCategories] = useState<ProductCategory[]>([])
  const [products, setProducts] = useState<any[]>([])
  const [categories, setCategories] = useState<any[]>([])
  const [isLoading, setIsLoading] = useState(true)
  const [isDialogOpen, setIsDialogOpen] = useState(false)
  const [isDeleteDialogOpen, setIsDeleteDialogOpen] = useState(false)
  const [currentMapping, setCurrentMapping] = useState<ProductCategory | null>(null)

  useEffect(() => {
    fetchProductCategories()
    fetchProducts()
    fetchCategories()
  }, [])

  const fetchProductCategories = async () => {
    setIsLoading(true)
    try {
      const data = await ProductCategoryAPI.getAll()

      // Fetch product and category names
      const mappingsWithNames = await Promise.all(
        data.map(async (mapping) => {
          try {
            const [product, category] = await Promise.all([
              ProductAPI.getBySku(mapping.productId),
              CategoryAPI.getById(mapping.categoryId),
            ])

            return {
              ...mapping,
              productName: product.name,
              categoryName: category.name,
            }
          } catch (error) {
            return {
              ...mapping,
              productName: "Unknown Product",
              categoryName: "Unknown Category",
            }
          }
        }),
      )

      setProductCategories(mappingsWithNames)
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to fetch product categories",
        variant: "destructive",
      })
    } finally {
      setIsLoading(false)
    }
  }

  const fetchProducts = async () => {
    try {
      const data = await ProductAPI.getAll()
      setProducts(data)
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to fetch products",
        variant: "destructive",
      })
    }
  }

  const fetchCategories = async () => {
    try {
      const data = await CategoryAPI.getAll()
      setCategories(data)
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to fetch categories",
        variant: "destructive",
      })
    }
  }

  const handleAddNew = () => {
    setCurrentMapping({
      productId: "",
      categoryId: 0,
    })
    setIsDialogOpen(true)
  }

  const handleDelete = (mapping: ProductCategory) => {
    setCurrentMapping(mapping)
    setIsDeleteDialogOpen(true)
  }

  const confirmDelete = async () => {
    if (!currentMapping) return

    try {
      await ProductCategoryAPI.delete(currentMapping.productId, currentMapping.categoryId)
      toast({
        title: "Success",
        description: "Product category mapping deleted successfully",
      })
      fetchProductCategories()
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to delete product category mapping",
        variant: "destructive",
      })
    } finally {
      setIsDeleteDialogOpen(false)
    }
  }

  const handleSelectChange = (name: string, value: string) => {
    if (!currentMapping) return

    setCurrentMapping({
      ...currentMapping,
      [name]: name === "categoryId" ? Number(value) : value,
    })
  }

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!currentMapping) return

    try {
      await ProductCategoryAPI.create(currentMapping)
      toast({
        title: "Success",
        description: "Product category mapping created successfully",
      })
      setIsDialogOpen(false)
      fetchProductCategories()
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to create product category mapping",
        variant: "destructive",
      })
    }
  }

  const columns: ColumnDef<ProductCategory>[] = [
    {
      accessorKey: "productId",
      header: "Product ID",
    },
    {
      accessorKey: "productName",
      header: "Product Name",
    },
    {
      accessorKey: "categoryId",
      header: "Category ID",
    },
    {
      accessorKey: "categoryName",
      header: "Category Name",
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
            <DropdownMenuItem onClick={() => handleDelete(row.original)}>
              <Trash className="mr-2 h-4 w-4" /> Delete
            </DropdownMenuItem>
          </DropdownMenuContent>
        </DropdownMenu>
      ),
    },
  ]

  return (
    <div className="space-y-4">
      <div className="flex justify-between items-center">
        <h1 className="text-2xl font-bold tracking-tight">Product Categories</h1>
        <Button onClick={handleAddNew}>
          <Plus className="mr-2 h-4 w-4" /> Add Mapping
        </Button>
      </div>

      <Card>
        <CardHeader>
          <CardTitle>Product Category Mappings</CardTitle>
          <CardDescription>Manage product to category relationships</CardDescription>
        </CardHeader>
        <CardContent>
          {isLoading ? (
            <div className="flex justify-center items-center h-24">Loading product categories...</div>
          ) : (
            <DataTable
              columns={columns}
              data={productCategories}
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
              <DialogTitle>Add Product to Category</DialogTitle>
              <DialogDescription>Create a new mapping between a product and a category</DialogDescription>
            </DialogHeader>
            <div className="grid gap-4 py-4">
              <div className="space-y-2">
                <Label htmlFor="productId">Product</Label>
                <Select
                  value={currentMapping?.productId}
                  onValueChange={(value) => handleSelectChange("productId", value)}
                >
                  <SelectTrigger>
                    <SelectValue placeholder="Select a product" />
                  </SelectTrigger>
                  <SelectContent>
                    {products.map((product) => (
                      <SelectItem key={product.sku} value={product.sku}>
                        {product.name}
                      </SelectItem>
                    ))}
                  </SelectContent>
                </Select>
              </div>
              <div className="space-y-2">
                <Label htmlFor="categoryId">Category</Label>
                <Select
                  value={currentMapping?.categoryId.toString()}
                  onValueChange={(value) => handleSelectChange("categoryId", value)}
                >
                  <SelectTrigger>
                    <SelectValue placeholder="Select a category" />
                  </SelectTrigger>
                  <SelectContent>
                    {categories.map((category) => (
                      <SelectItem key={category.categoryId} value={category.categoryId.toString()}>
                        {category.name}
                      </SelectItem>
                    ))}
                  </SelectContent>
                </Select>
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
              Are you sure you want to delete this product category mapping? This action cannot be undone.
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
