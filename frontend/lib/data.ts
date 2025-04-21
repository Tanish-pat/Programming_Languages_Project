import type { InventoryItem } from "@/components/inventory-columns"
import type { CategoryItem } from "@/components/category-columns"
import type { SupplierItem } from "@/components/supplier-columns"
import type { OrderItem } from "@/components/order-columns"
import type { CustomerItem } from "@/components/customer-columns"

export async function getInventoryData(): Promise<InventoryItem[]> {
  // In a real app, this would fetch from an API
  return [
    {
      id: 1,
      name: "Laptop",
      category: "Electronics",
      supplier: "Acme Inc.",
      quantity: 25,
      price: 999.99,
    },
    {
      id: 2,
      name: "Office Chair",
      category: "Furniture",
      supplier: "Globex Corporation",
      quantity: 15,
      price: 199.99,
    },
    {
      id: 3,
      name: "Wireless Mouse",
      category: "Electronics",
      supplier: "Acme Inc.",
      quantity: 50,
      price: 29.99,
    },
    {
      id: 4,
      name: "Desk Lamp",
      category: "Furniture",
      supplier: "Initech",
      quantity: 30,
      price: 49.99,
    },
    {
      id: 5,
      name: "Printer Paper",
      category: "Office Supplies",
      supplier: "Umbrella Corp",
      quantity: 100,
      price: 9.99,
    },
    {
      id: 6,
      name: "Ink Cartridges",
      category: "Office Supplies",
      supplier: "Umbrella Corp",
      quantity: 20,
      price: 24.99,
    },
    {
      id: 7,
      name: "USB Drives",
      category: "Electronics",
      supplier: "Initech",
      quantity: 40,
      price: 12.99,
    },
    {
      id: 8,
      name: "Desk",
      category: "Furniture",
      supplier: "Globex Corporation",
      quantity: 10,
      price: 299.99,
    },
    {
      id: 9,
      name: "Headphones",
      category: "Electronics",
      supplier: "Acme Inc.",
      quantity: 35,
      price: 79.99,
    },
    {
      id: 10,
      name: "Notebook",
      category: "Office Supplies",
      supplier: "Umbrella Corp",
      quantity: 200,
      price: 4.99,
    },
  ]
}

export async function getCategoryData(): Promise<CategoryItem[]> {
  return [
    {
      id: 1,
      name: "Electronics",
      description: "Electronic devices and accessories",
      itemCount: 45,
    },
    {
      id: 2,
      name: "Furniture",
      description: "Office and home furniture",
      itemCount: 32,
    },
    {
      id: 3,
      name: "Office Supplies",
      description: "General office supplies and stationery",
      itemCount: 78,
    },
    {
      id: 4,
      name: "Books",
      description: "Books and publications",
      itemCount: 12,
    },
    {
      id: 5,
      name: "Food & Beverages",
      description: "Consumable items",
      itemCount: 23,
    },
  ]
}

export async function getSupplierData(): Promise<SupplierItem[]> {
  return [
    {
      id: 1,
      name: "Acme Inc.",
      contact: "John Smith",
      email: "john@acme.com",
      phone: "555-123-4567",
      itemCount: 28,
    },
    {
      id: 2,
      name: "Globex Corporation",
      contact: "Jane Doe",
      email: "jane@globex.com",
      phone: "555-987-6543",
      itemCount: 15,
    },
    {
      id: 3,
      name: "Initech",
      contact: "Michael Johnson",
      email: "michael@initech.com",
      phone: "555-456-7890",
      itemCount: 32,
    },
    {
      id: 4,
      name: "Umbrella Corp",
      contact: "Sarah Williams",
      email: "sarah@umbrella.com",
      phone: "555-789-0123",
      itemCount: 19,
    },
  ]
}

export async function getOrderData(): Promise<OrderItem[]> {
  return [
    {
      id: 1,
      customer: "John Doe",
      date: "2023-04-15",
      status: "Completed",
      total: 1999.99,
      items: 3,
    },
    {
      id: 2,
      customer: "Jane Smith",
      date: "2023-04-14",
      status: "Processing",
      total: 1499.0,
      items: 2,
    },
    {
      id: 3,
      customer: "Robert Johnson",
      date: "2023-04-13",
      status: "Completed",
      total: 899.0,
      items: 5,
    },
    {
      id: 4,
      customer: "Sarah Davis",
      date: "2023-04-12",
      status: "Shipped",
      total: 3499.0,
      items: 7,
    },
    {
      id: 5,
      customer: "Michael Brown",
      date: "2023-04-11",
      status: "Pending",
      total: 699.0,
      items: 1,
    },
  ]
}

export async function getCustomerData(): Promise<CustomerItem[]> {
  return [
    {
      id: 1,
      name: "John Doe",
      email: "john.doe@example.com",
      phone: "555-123-4567",
      orders: 8,
      totalSpent: 12580.45,
    },
    {
      id: 2,
      name: "Jane Smith",
      email: "jane.smith@example.com",
      phone: "555-987-6543",
      orders: 5,
      totalSpent: 7890.3,
    },
    {
      id: 3,
      name: "Robert Johnson",
      email: "robert.johnson@example.com",
      phone: "555-456-7890",
      orders: 12,
      totalSpent: 15670.8,
    },
    {
      id: 4,
      name: "Sarah Davis",
      email: "sarah.davis@example.com",
      phone: "555-789-0123",
      orders: 3,
      totalSpent: 4320.15,
    },
    {
      id: 5,
      name: "Michael Brown",
      email: "michael.brown@example.com",
      phone: "555-234-5678",
      orders: 7,
      totalSpent: 9540.6,
    },
  ]
}
