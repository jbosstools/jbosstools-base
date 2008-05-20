/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.attribute.adapter;

import java.util.*;
import org.eclipse.core.runtime.IAdaptable;
import org.jboss.tools.common.model.ui.IStructuredChangeListener;
import org.jboss.tools.common.model.ui.StructuredChangedEvent;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.jboss.tools.common.model.ui.actions.IActionProvider;

/**
 * @author au
 */
public class DefaultTableStructuredAdapter implements IAdaptable, ITableAdapter, ISelectionChangedListener, ISelectionProvider {

    private IActionProvider actionProvider;
    private List<ColumnDescription> columnsDescription = new ArrayList<ColumnDescription>();

    public Object getAdapter(Class adapter) {
		if (adapter == IActionProvider.class) {
			return getActionProvider();
		}
		if (adapter == ITableAdapter.class) {
			return this;
		}
		if (adapter == ISelectionProvider.class) {
			return this;
		}
		return null;
    }

    /*
     * Warning! The @style attribute cannot use in this method.
     * @see org.jboss.tools.common.model.ui.attribute.adapter.ITableAdapter#createTableColumn(org.eclipse.swt.widgets.Table, int)
     */
    public TableColumn[] createTableColumn(Table table, int style) {
        List<TableColumn> columns = new ArrayList<TableColumn>();
        Iterator i = getColumnsDescription().iterator();
        TableLayout layout = new TableLayout();
        ColumnDescription description;
        TableColumn column;
        ColumnLayoutData layoutData;
        while (i.hasNext()) {
            description = (ColumnDescription)i.next();
            column = new TableColumn(table, description.getStyle());
            column.setText(description.getName());
            column.setData(description.getData());
            //column.setResizable(description.isResizeable());
            //column.setWidth(description.getWidth());
            column.setImage(description.getImage());
			layoutData = new ColumnWeightData(description.getWidth(), description.isResizeable());
			layout.addColumnData(layoutData);
			columns.add(column);
        }
        table.setLayout(layout);
        
//		if(getColumnsDescription().size() < 4) {
//			// Cannot guarantee nice work for many columns (glory) 
//			table.addControlListener(new Resize(table));
//		}
        
		return (TableColumn[])columns.toArray(new TableColumn[columns.size()]);
    }

	private ArrayList<IStructuredChangeListener> structureChangeListener = new ArrayList<IStructuredChangeListener>();
    
	public void addStructureChangeListener(IStructuredChangeListener listener) {
		structureChangeListener.add(listener);
	}

	public void removeStructureChangeListener(IStructuredChangeListener listener) {
		structureChangeListener.remove(listener);
	}

	protected void fireStructureChange() {
//		updateValue();
		ArrayList<IStructuredChangeListener> copy = new ArrayList<IStructuredChangeListener>();
		copy.addAll(structureChangeListener);
		Iterator i = copy.iterator();
		while(i.hasNext()){
			((IStructuredChangeListener)i.next()).structureChanged(new StructuredChangedEvent(this));
		}
		copy.clear();
		
//		if (actionProvider!=null) {
//			actionProvider.setXModelObject(selectedObject);
//		}
	}
	
	private ITableLabelProvider tableLabelProvider = new DefaultTableLabelProvider();
	
    public ITableLabelProvider getTableLabelProvider() {
        return tableLabelProvider;
    }

    public void setTableLabelProvider(ITableLabelProvider tableLabelProvider) {
        this.tableLabelProvider = tableLabelProvider;
    }

    private Object[] value;
    
    public Object[] getElements(Object inputElement) {
	    if (this.value==null) return new Object[0];
		return (Object[])this.value;
    }

    public void dispose() {
    }

    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
    }

    public Color getForeground(Object element) {
        return null;
    }

    public Color getBackground(Object element) {
        return null;
    }

//	private SelectionChangedEvent selectionChangedEvent;
	private Object selectedObject;
    
    public void selectionChanged(SelectionChangedEvent event) {
//		this.selectionChangedEvent = event;
		if (event.getSelection() instanceof StructuredSelection) {
			StructuredSelection structuredSelection = (StructuredSelection)event.getSelection();
			Object object = structuredSelection.getFirstElement();
			this.selectedObject = object;
		}
		this.fireSelectionChange();
    }

	// *****************************
	// * ISelectionProvider
	// *****************************
	private ArrayList<ISelectionChangedListener> selectionChangeListener = new ArrayList<ISelectionChangedListener>();
	private ISelection selection;

	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		selectionChangeListener.add(listener);
	}

	public ISelection getSelection() {
		if(selectedObject == null) {
			selection = new StructuredSelection();
		} else {
			selection = new StructuredSelection(new Object[] {this.selectedObject});
		}
		return selection;
	}

	public void removeSelectionChangedListener(ISelectionChangedListener listener) {
		selectionChangeListener.remove(listener);
	}

	public void setSelection(ISelection selection) {
		this.selection = selection;
	}

	protected void fireSelectionChange() {
		ArrayList<ISelectionChangedListener> copy = new ArrayList<ISelectionChangedListener>(selectionChangeListener);
		Iterator i = copy.iterator();
		while(i.hasNext()){
			((ISelectionChangedListener)i.next()).selectionChanged(new SelectionChangedEvent(this, getSelection()));
		}
		copy.clear();
	}

    public IActionProvider getActionProvider() {
        return actionProvider;
    }
    
    public void setActionProvider(IActionProvider actionProvider) {
        this.actionProvider = actionProvider;
    }
    
    public List<ColumnDescription> getColumnsDescription() {
        return columnsDescription;
    }
    
    public void addColumnDescription(ColumnDescription description) {
        this.getColumnsDescription().add(description);
    }
    
    
    public Object[] getValue() {
        return value;
    }
    public void setValue(Object[] value) {
    	if(this.value != value) {
    		selectedObject = null;
    		if(actionProvider != null) {
   				actionProvider.update(getSelection());
    		}
    	}
        this.value = value;        
        fireStructureChange();
    }

	class DefaultTableLabelProvider implements ITableLabelProvider {

		public Image getColumnImage(Object element, int columnIndex) {
			return null;
		}

		public String getColumnText(Object element, int columnIndex) {
		    return element.toString();
		}

		public void addListener(ILabelProviderListener listener) {
		}

		public void dispose() {
		}

		public boolean isLabelProperty(Object element, String property) {
			return Boolean.TRUE.booleanValue();
		}

		public void removeListener(ILabelProviderListener listener) {
		}
	}

}
