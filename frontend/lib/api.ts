// This file contains functions to interact with the Haskell backend API

// Base URL for the API
const API_BASE_URL = "http://localhost:8080"

// ==================== DASHBOARD API ====================

/**
 * Fetch dashboard statistics
 *
 * API Endpoint: GET /api/stats
 * Returns statistics about inventory, categories, suppliers, etc.
 */
export async function fetchStats() {
  try {
    // In a real implementation, we would fetch from the actual API
    // const response = await fetch(`${API_BASE_URL}/api/stats`);
    // if (!response.ok) throw new Error('Failed to fetch stats');
    // return await response.json();

    // For now, return mock data
    return {
      totalItems: 156,
      totalCategories: 8,
      totalSuppliers: 12,
      lowStockItems: 5,
      totalValue: 45678.99,
    }
  } catch (error) {
    console.error("Error fetching stats:", error)
    throw error
  }
}

/**
 * Fetch recent inventory items
 *
 * API Endpoint: GET /api/items/recent
 * Returns the most recently added inventory items
 */
export async function fetchRecentItems() {
  try {
    // In a real implementation, we would fetch from the actual API
    // const response = await fetch(`${API_BASE_URL}/api/items/recent`);
    // if (!response.ok) throw new Error('Failed to fetch recent items');
    // return await response.json();

    // For now, return mock data
    return [
      {
        id: 1,
        name: "Laptop",
        quantity: 25,
        price: 999.99,
        category: "Electronics",
        createdAt: "2023-04-15T10:30:00Z",
      },
      {
        id: 2,
        name: "Office Chair",
        quantity: 15,
        price: 199.99,
        category: "Furniture",
        createdAt: "2023-04-14T14:20:00Z",
      },
      {
        id: 3,
        name: "Wireless Mouse",
        quantity: 50,
        price: 29.99,
        category: "Electronics",
        createdAt: "2023-04-13T09:15:00Z",
      },
      {
        id: 4,
        name: "Desk Lamp",
        quantity: 30,
        price: 49.99,
        category: "Furniture",
        createdAt: "2023-04-12T16:45:00Z",
      },
    ]
  } catch (error) {
    console.error("Error fetching recent items:", error)
    throw error
  }
}

/**
 * Fetch low stock items
 *
 * API Endpoint: GET /api/items/low-stock
 * Returns items that are below their threshold levels
 */
export async function fetchLowStockItems() {
  try {
    // In a real implementation, we would fetch from the actual API
    // const response = await fetch(`${API_BASE_URL}/api/items/low-stock`);
    // if (!response.ok) throw new Error('Failed to fetch low stock items');
    // return await response.json();

    // For now, return mock data
    return [
      {
        id: 5,
        name: "Printer Paper",
        quantity: 3,
        threshold: 10,
      },
      {
        id: 6,
        name: "Ink Cartridges",
        quantity: 2,
        threshold: 5,
      },
      {
        id: 7,
        name: "USB Drives",
        quantity: 4,
        threshold: 15,
      },
    ]
  } catch (error) {
    console.error("Error fetching low stock items:", error)
    throw error
  }
}

/**
 * Fetch recent activity
 *
 * API Endpoint: GET /api/activity/recent
 * Returns recent system activity like adding/editing items, etc.
 */
export async function fetchRecentActivity() {
  try {
    // In a real implementation, we would fetch from the actual API
    // const response = await fetch(`${API_BASE_URL}/api/activity/recent`);
    // if (!response.ok) throw new Error('Failed to fetch recent activity');
    // return await response.json();

    // For now, return mock data
    return [
      {
        id: 1,
        type: "add",
        entityType: "item",
        entityId: 1,
        entityName: "Laptop",
        user: "John Doe",
        timestamp: "2023-04-15T10:30:00Z",
      },
      {
        id: 2,
        type: "edit",
        entityType: "item",
        entityId: 3,
        entityName: "Wireless Mouse",
        user: "Jane Smith",
        timestamp: "2023-04-14T15:45:00Z",
        details: "Updated quantity from 45 to 50",
      },
      {
        id: 3,
        type: "delete",
        entityType: "item",
        entityId: 8,
        entityName: "Headphones",
        user: "John Doe",
        timestamp: "2023-04-13T09:20:00Z",
      },
      {
        id: 4,
        type: "restock",
        entityType: "item",
        entityId: 5,
        entityName: "Printer Paper",
        user: "Jane Smith",
        timestamp: "2023-04-12T14:10:00Z",
        details: "Added 20 units",
      },
      {
        id: 5,
        type: "add",
        entityType: "supplier",
        entityId: 3,
        entityName: "Tech Solutions Inc.",
        user: "John Doe",
        timestamp: "2023-04-11T11:30:00Z",
      },
    ]
  } catch (error) {
    console.error("Error fetching recent activity:", error)
    throw error
  }
}

// ==================== INVENTORY API ====================

/**
 * Fetch all inventory items with pagination
 *
 * API Endpoint: GET /api/items?page={page}&pageSize={pageSize}
 * Returns paginated list of inventory items
 */
export async function fetchInventory(page = 1, pageSize = 10) {
  try {
    // In a real implementation, we would fetch from the actual API
    // const response = await fetch(`${API_BASE_URL}/api/items?page=${page}&pageSize=${pageSize}`);
    // if (!response.ok) throw new Error('Failed to fetch inventory');
    // return await response.json();

    // For now, return mock data
    const items = [
      {
        id: 1,
        name: "Laptop",
        description: "High-performance laptop for professional use",
        quantity: 25,
        price: 999.99,
        category: "Electronics",
        supplier: "Acme Inc.",
        threshold: 10,
      },
      {
        id: 2,
        name: "Office Chair",
        description: "Ergonomic office chair with lumbar support",
        quantity: 15,
        price: 199.99,
        category: "Furniture",
        supplier: "Globex Corporation",
        threshold: 5,
      },
      {
        id: 3,
        name: "Wireless Mouse",
        description: "Bluetooth wireless mouse with long battery life",
        quantity: 50,
        price: 29.99,
        category: "Electronics",
        supplier: "Acme Inc.",
        threshold: 15,
      },
      {
        id: 4,
        name: "Desk Lamp",
        description: "LED desk lamp with adjustable brightness",
        quantity: 30,
        price: 49.99,
        category: "Furniture",
        supplier: "Initech",
        threshold: 10,
      },
      {
        id: 5,
        name: "Printer Paper",
        description: "A4 printer paper, 500 sheets per pack",
        quantity: 3,
        price: 9.99,
        category: "Office Supplies",
        supplier: "Umbrella Corp",
        threshold: 10,
      },
      {
        id: 6,
        name: "Ink Cartridges",
        description: "Compatible ink cartridges for HP printers",
        quantity: 2,
        price: 24.99,
        category: "Office Supplies",
        supplier: "Umbrella Corp",
        threshold: 5,
      },
      {
        id: 7,
        name: "USB Drives",
        description: "16GB USB flash drives",
        quantity: 4,
        price: 12.99,
        category: "Electronics",
        supplier: "Initech",
        threshold: 15,
      },
    ]

    return {
      items,
      total: 156,
    }
  } catch (error) {
    console.error("Error fetching inventory:", error)
    throw error
  }
}

/**
 * Fetch a single inventory item by ID
 *
 * API Endpoint: GET /api/items/{id}
 * Returns detailed information about a specific inventory item
 */
export async function fetchInventoryItem(id: number) {
  try {
    // In a real implementation, we would fetch from the actual API
    // const response = await fetch(`${API_BASE_URL}/api/items/${id}`);
    // if (!response.ok) throw new Error('Failed to fetch inventory item');
    // return await response.json();

    // For now, return mock data based on the ID
    const items = {
      1: {
        id: 1,
        name: "Laptop",
        description: "High-performance laptop for professional use",
        quantity: 25,
        price: 999.99,
        category: "Electronics",
        categoryId: "1",
        supplier: "Acme Inc.",
        supplierId: "1",
        threshold: 10,
        createdAt: "2023-04-15T10:30:00Z",
        updatedAt: "2023-04-15T10:30:00Z",
      },
      2: {
        id: 2,
        name: "Office Chair",
        description: "Ergonomic office chair with lumbar support",
        quantity: 15,
        price: 199.99,
        category: "Furniture",
        categoryId: "2",
        supplier: "Globex Corporation",
        supplierId: "2",
        threshold: 5,
        createdAt: "2023-04-14T14:20:00Z",
        updatedAt: "2023-04-14T14:20:00Z",
      },
    }

    return items[id] || items[1] // Return the item with the given ID or default to the first item
  } catch (error) {
    console.error("Error fetching inventory item:", error)
    throw error
  }
}

/**
 * Add a new inventory item
 *
 * API Endpoint: POST /api/items
 * Creates a new inventory item with the provided data
 */
export async function addInventoryItem(itemData: any) {
  try {
    // In a real implementation, we would post to the actual API
    // const response = await fetch(`${API_BASE_URL}/api/items`, {
    //   method: 'POST',
    //   headers: {
    //     'Content-Type': 'application/json',
    //   },
    //   body: JSON.stringify(itemData),
    // });
    // if (!response.ok) throw new Error('Failed to add inventory item');
    // return await response.json();

    // For now, just log and return mock success
    console.log("Adding inventory item:", itemData)
    return { success: true, id: Math.floor(Math.random() * 1000) }
  } catch (error) {
    console.error("Error adding inventory item:", error)
    throw error
  }
}

/**
 * Update an existing inventory item
 *
 * API Endpoint: PUT /api/items/{id}
 * Updates an existing inventory item with the provided data
 */
export async function updateInventoryItem(id: number, itemData: any) {
  try {
    // In a real implementation, we would put to the actual API
    // const response = await fetch(`${API_BASE_URL}/api/items/${id}`, {
    //   method: 'PUT',
    //   headers: {
    //     'Content-Type': 'application/json',
    //   },
    //   body: JSON.stringify(itemData),
    // });
    // if (!response.ok) throw new Error('Failed to update inventory item');
    // return await response.json();

    // For now, just log and return mock success
    console.log(`Updating inventory item ${id}:`, itemData)
    return { success: true }
  } catch (error) {
    console.error("Error updating inventory item:", error)
    throw error
  }
}

/**
 * Delete an inventory item
 *
 * API Endpoint: DELETE /api/items/{id}
 * Deletes an inventory item with the specified ID
 */
export async function deleteInventoryItem(id: number) {
  try {
    // In a real implementation, we would delete from the actual API
    // const response = await fetch(`${API_BASE_URL}/api/items/${id}`, {
    //   method: 'DELETE',
    // });
    // if (!response.ok) throw new Error('Failed to delete inventory item');
    // return await response.json();

    // For now, just log and return mock success
    console.log(`Deleting inventory item ${id}`)
    return { success: true }
  } catch (error) {
    console.error("Error deleting inventory item:", error)
    throw error
  }
}

// ==================== CATEGORIES API ====================

/**
 * Fetch all categories with pagination
 *
 * API Endpoint: GET /api/categories?page={page}&pageSize={pageSize}
 * Returns paginated list of categories
 */
export async function fetchCategories(page = 1, pageSize = 10) {
  try {
    // In a real implementation, we would fetch from the actual API
    // const response = await fetch(`${API_BASE_URL}/api/categories?page=${page}&pageSize=${pageSize}`);
    // if (!response.ok) throw new Error('Failed to fetch categories');
    // return await response.json();

    // For now, return mock data
    const categories = [
      {
        id: 1,
        name: "Electronics",
        description: "Electronic devices and accessories",
        itemCount: 45,
        createdAt: "2023-01-15T10:30:00Z",
      },
      {
        id: 2,
        name: "Furniture",
        description: "Office and home furniture",
        itemCount: 32,
        createdAt: "2023-01-16T11:20:00Z",
      },
      {
        id: 3,
        name: "Office Supplies",
        description: "General office supplies and stationery",
        itemCount: 78,
        createdAt: "2023-01-17T09:15:00Z",
      },
      {
        id: 4,
        name: "Books",
        description: "Books and publications",
        itemCount: 12,
        createdAt: "2023-01-18T14:45:00Z",
      },
      {
        id: 5,
        name: "Food & Beverages",
        description: "Consumable items",
        itemCount: 23,
        createdAt: "2023-01-19T16:30:00Z",
      },
    ]

    return {
      categories,
      total: 8,
    }
  } catch (error) {
    console.error("Error fetching categories:", error)
    throw error
  }
}

/**
 * Fetch a single category by ID
 *
 * API Endpoint: GET /api/categories/{id}
 * Returns detailed information about a specific category
 */
export async function fetchCategory(id: number) {
  try {
    // In a real implementation, we would fetch from the actual API
    // const response = await fetch(`${API_BASE_URL}/api/categories/${id}`);
    // if (!response.ok) throw new Error('Failed to fetch category');
    // return await response.json();

    // For now, return mock data based on the ID
    const categories = {
      1: {
        id: 1,
        name: "Electronics",
        description: "Electronic devices and accessories",
        itemCount: 45,
        createdAt: "2023-01-15T10:30:00Z",
        updatedAt: "2023-01-15T10:30:00Z",
      },
      2: {
        id: 2,
        name: "Furniture",
        description: "Office and home furniture",
        itemCount: 32,
        createdAt: "2023-01-16T11:20:00Z",
        updatedAt: "2023-01-16T11:20:00Z",
      },
    }

    return categories[id] || categories[1] // Return the category with the given ID or default to the first category
  } catch (error) {
    console.error("Error fetching category:", error)
    throw error
  }
}

/**
 * Add a new category
 *
 * API Endpoint: POST /api/categories
 * Creates a new category with the provided data
 */
export async function addCategory(categoryData: any) {
  try {
    // In a real implementation, we would post to the actual API
    // const response = await fetch(`${API_BASE_URL}/api/categories`, {
    //   method: 'POST',
    //   headers: {
    //     'Content-Type': 'application/json',
    //   },
    //   body: JSON.stringify(categoryData),
    // });
    // if (!response.ok) throw new Error('Failed to add category');
    // return await response.json();

    // For now, just log and return mock success
    console.log("Adding category:", categoryData)
    return { success: true, id: Math.floor(Math.random() * 1000) }
  } catch (error) {
    console.error("Error adding category:", error)
    throw error
  }
}

/**
 * Update an existing category
 *
 * API Endpoint: PUT /api/categories/{id}
 * Updates an existing category with the provided data
 */
export async function updateCategory(id: number, categoryData: any) {
  try {
    // In a real implementation, we would put to the actual API
    // const response = await fetch(`${API_BASE_URL}/api/categories/${id}`, {
    //   method: 'PUT',
    //   headers: {
    //     'Content-Type': 'application/json',
    //   },
    //   body: JSON.stringify(categoryData),
    // });
    // if (!response.ok) throw new Error('Failed to update category');
    // return await response.json();

    // For now, just log and return mock success
    console.log(`Updating category ${id}:`, categoryData)
    return { success: true }
  } catch (error) {
    console.error("Error updating category:", error)
    throw error
  }
}

/**
 * Delete a category
 *
 * API Endpoint: DELETE /api/categories/{id}
 * Deletes a category with the specified ID
 */
export async function deleteCategory(id: number) {
  try {
    // In a real implementation, we would delete from the actual API
    // const response = await fetch(`${API_BASE_URL}/api/categories/${id}`, {
    //   method: 'DELETE',
    // });
    // if (!response.ok) throw new Error('Failed to delete category');
    // return await response.json();

    // For now, just log and return mock success
    console.log(`Deleting category ${id}`)
    return { success: true }
  } catch (error) {
    console.error("Error deleting category:", error)
    throw error
  }
}

/**
 * Fetch items in a category
 *
 * API Endpoint: GET /api/categories/{id}/items?page={page}&pageSize={pageSize}
 * Returns paginated list of items in a specific category
 */
export async function fetchCategoryItems(id: number, page = 1, pageSize = 10) {
  try {
    // In a real implementation, we would fetch from the actual API
    // const response = await fetch(`${API_BASE_URL}/api/categories/${id}/items?page=${page}&pageSize=${pageSize}`);
    // if (!response.ok) throw new Error('Failed to fetch category items');
    // return await response.json();

    // For now, return mock data
    const items = [
      {
        id: 1,
        name: "Laptop",
        description: "High-performance laptop for professional use",
        quantity: 25,
        price: 999.99,
        supplier: "Acme Inc.",
        threshold: 10,
      },
      {
        id: 3,
        name: "Wireless Mouse",
        description: "Bluetooth wireless mouse with long battery life",
        quantity: 50,
        price: 29.99,
        supplier: "Acme Inc.",
        threshold: 15,
      },
      {
        id: 7,
        name: "USB Drives",
        description: "16GB USB flash drives",
        quantity: 4,
        price: 12.99,
        supplier: "Initech",
        threshold: 15,
      },
    ]

    return {
      items,
      total: 45,
    }
  } catch (error) {
    console.error("Error fetching category items:", error)
    throw error
  }
}

// ==================== SUPPLIERS API ====================

/**
 * Fetch all suppliers with pagination
 *
 * API Endpoint: GET /api/suppliers?page={page}&pageSize={pageSize}
 * Returns paginated list of suppliers
 */
export async function fetchSuppliers(page = 1, pageSize = 10) {
  try {
    // In a real implementation, we would fetch from the actual API
    // const response = await fetch(`${API_BASE_URL}/api/suppliers?page=${page}&pageSize=${pageSize}`);
    // if (!response.ok) throw new Error('Failed to fetch suppliers');
    // return await response.json();

    // For now, return mock data
    const suppliers = [
      {
        id: 1,
        name: "Acme Inc.",
        contact: "John Smith",
        email: "john@acme.com",
        phone: "555-123-4567",
        address: "123 Main St, Anytown, USA",
        itemCount: 28,
        createdAt: "2023-01-10T09:30:00Z",
      },
      {
        id: 2,
        name: "Globex Corporation",
        contact: "Jane Doe",
        email: "jane@globex.com",
        phone: "555-987-6543",
        address: "456 Oak Ave, Somewhere, USA",
        itemCount: 15,
        createdAt: "2023-01-11T10:45:00Z",
      },
      {
        id: 3,
        name: "Initech",
        contact: "Michael Johnson",
        email: "michael@initech.com",
        phone: "555-456-7890",
        address: "789 Pine Rd, Nowhere, USA",
        itemCount: 32,
        createdAt: "2023-01-12T11:15:00Z",
      },
      {
        id: 4,
        name: "Umbrella Corp",
        contact: "Sarah Williams",
        email: "sarah@umbrella.com",
        phone: "555-789-0123",
        address: "321 Elm St, Everywhere, USA",
        itemCount: 19,
        createdAt: "2023-01-13T14:20:00Z",
      },
    ]

    return {
      suppliers,
      total: 12,
    }
  } catch (error) {
    console.error("Error fetching suppliers:", error)
    throw error
  }
}

/**
 * Fetch a single supplier by ID
 *
 * API Endpoint: GET /api/suppliers/{id}
 * Returns detailed information about a specific supplier
 */
export async function fetchSupplier(id: number) {
  try {
    // In a real implementation, we would fetch from the actual API
    // const response = await fetch(`${API_BASE_URL}/api/suppliers/${id}`);
    // if (!response.ok) throw new Error('Failed to fetch supplier');
    // return await response.json();

    // For now, return mock data based on the ID
    const suppliers = {
      1: {
        id: 1,
        name: "Acme Inc.",
        contact: "John Smith",
        email: "john@acme.com",
        phone: "555-123-4567",
        address: "123 Main St, Anytown, USA",
        itemCount: 28,
        createdAt: "2023-01-10T09:30:00Z",
        updatedAt: "2023-01-10T09:30:00Z",
      },
      2: {
        id: 2,
        name: "Globex Corporation",
        contact: "Jane Doe",
        email: "jane@globex.com",
        phone: "555-987-6543",
        address: "456 Oak Ave, Somewhere, USA",
        itemCount: 15,
        createdAt: "2023-01-11T10:45:00Z",
        updatedAt: "2023-01-11T10:45:00Z",
      },
    }

    return suppliers[id] || suppliers[1] // Return the supplier with the given ID or default to the first supplier
  } catch (error) {
    console.error("Error fetching supplier:", error)
    throw error
  }
}

/**
 * Add a new supplier
 *
 * API Endpoint: POST /api/suppliers
 * Creates a new supplier with the provided data
 */
export async function addSupplier(supplierData: any) {
  try {
    // In a real implementation, we would post to the actual API
    // const response = await fetch(`${API_BASE_URL}/api/suppliers`, {
    //   method: 'POST',
    //   headers: {
    //     'Content-Type': 'application/json',
    //   },
    //   body: JSON.stringify(supplierData),
    // });
    // if (!response.ok) throw new Error('Failed to add supplier');
    // return await response.json();

    // For now, just log and return mock success
    console.log("Adding supplier:", supplierData)
    return { success: true, id: Math.floor(Math.random() * 1000) }
  } catch (error) {
    console.error("Error adding supplier:", error)
    throw error
  }
}

/**
 * Update an existing supplier
 *
 * API Endpoint: PUT /api/suppliers/{id}
 * Updates an existing supplier with the provided data
 */
export async function updateSupplier(id: number, supplierData: any) {
  try {
    // In a real implementation, we would put to the actual API
    // const response = await fetch(`${API_BASE_URL}/api/suppliers/${id}`, {
    //   method: 'PUT',
    //   headers: {
    //     'Content-Type': 'application/json',
    //   },
    //   body: JSON.stringify(supplierData),
    // });
    // if (!response.ok) throw new Error('Failed to update supplier');
    // return await response.json();

    // For now, just log and return mock success
    console.log(`Updating supplier ${id}:`, supplierData)
    return { success: true }
  } catch (error) {
    console.error("Error updating supplier:", error)
    throw error
  }
}

/**
 * Delete a supplier
 *
 * API Endpoint: DELETE /api/suppliers/{id}
 * Deletes a supplier with the specified ID
 */
export async function deleteSupplier(id: number) {
  try {
    // In a real implementation, we would delete from the actual API
    // const response = await fetch(`${API_BASE_URL}/api/suppliers/${id}`, {
    //   method: 'DELETE',
    // });
    // if (!response.ok) throw new Error('Failed to delete supplier');
    // return await response.json();

    // For now, just log and return mock success
    console.log(`Deleting supplier ${id}`)
    return { success: true }
  } catch (error) {
    console.error("Error deleting supplier:", error)
    throw error
  }
}

/**
 * Fetch items from a supplier
 *
 * API Endpoint: GET /api/suppliers/{id}/items?page={page}&pageSize={pageSize}
 * Returns paginated list of items from a specific supplier
 */
export async function fetchSupplierItems(id: number, page = 1, pageSize = 10) {
  try {
    // In a real implementation, we would fetch from the actual API
    // const response = await fetch(`${API_BASE_URL}/api/suppliers/${id}/items?page=${page}&pageSize=${pageSize}`);
    // if (!response.ok) throw new Error('Failed to fetch supplier items');
    // return await response.json();

    // For now, return mock data
    const items = [
      {
        id: 1,
        name: "Laptop",
        description: "High-performance laptop for professional use",
        quantity: 25,
        price: 999.99,
        category: "Electronics",
        threshold: 10,
      },
      {
        id: 3,
        name: "Wireless Mouse",
        description: "Bluetooth wireless mouse with long battery life",
        quantity: 50,
        price: 29.99,
        category: "Electronics",
        threshold: 15,
      },
    ]

    return {
      items,
      total: 28,
    }
  } catch (error) {
    console.error("Error fetching supplier items:", error)
    throw error
  }
}
