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
package org.jboss.tools.common.model.ui.editors.dnd.composite;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnPixelData;
import org.eclipse.jface.viewers.ICellEditorListener;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.IElementComparer;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.jboss.tools.common.model.ui.editors.dnd.IDropWizardModel;
import org.jboss.tools.common.model.ui.editors.dnd.DropUtils.AttributeDescriptorValueProvider;
import org.jboss.tools.common.model.ui.objecteditor.ExtendedCellEditorProvider;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

/**
 * 
 * @author eskimo
 *
 */

public class TagAttributesComposite extends Composite implements PropertyChangeListener, SelectionListener {
	private IDropWizardModel fWizardModel; 
	private TableViewer tableViewer = null;
	private boolean fFiltered = false;
	
	/**
	 * @author eskimo
	 */
	public static class AttributeDescriptorValue {
		String name;
		boolean isRequired;
		boolean isPreferable;
		/**
		 * 
		 * @param descriptor
		 */
		public AttributeDescriptorValue(String name, boolean isRequired, boolean isPreferable) {
			this.name = name;
			this.isPreferable = isPreferable;
			this.isRequired = isRequired;
		}
		
		/**
		 * 
		 */
		private Object fValue;
		
		/**
		 * 
		 * @return
		 */
		public Object getValue() {
			return fValue;
		}
		
		/**
		 * 
		 * @param value
		 */
		public void setValue(Object value) {
			fValue = value;
		}

		/**
		 * 
		 */
		public String getName() {
			return name;
		}

		/**
		 * 
		 */
		public boolean isPreferable() {
			return isPreferable;
		}

		public void setPreferable(boolean b) {
			isPreferable = b;
		}
		/**
		 * 
		 */
		public boolean isRequired() {
			return isRequired;
		}

		/**
		 * 
		 */
		public boolean equals(Object obj) {
			return super.equals(obj);
		}

		/**
		 * 
		 */
		public int hashCode() {
			return super.hashCode();
		}

	}

	/**
	 * 
	 * @param parent
	 * @param style
	 * @param wizardModel
	 */
	public TagAttributesComposite(Composite parent, int style, IDropWizardModel wizardModel) {
		this(parent, style, wizardModel, false); 
	}	
	
	/**
	 * 
	 * @param parent
	 * @param style
	 * @param wizardModel
	 */
	public TagAttributesComposite(Composite parent, int style, IDropWizardModel wizardModel,boolean filtered) {
		super(parent, style); 
		fFiltered = filtered;
		fWizardModel = wizardModel;
		fWizardModel.addPropertyChangeListener(this);
		addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				fWizardModel.removePropertyChangeListener(TagAttributesComposite.this);
				if (context != null) {
					context.clear();
					context = null;
				}
				removeDisposeListener(this);
			}
		});
		AttributeDescriptorValueProvider valueProvider = fWizardModel.getDropData().getValueProvider();
		if(valueProvider != null) {
			valueProvider.initContext(context);
		}
		GridLayout layout = new GridLayout();
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		layout.numColumns = 1;
		setLayout(layout);

		GridData data = new GridData(GridData.FILL_BOTH);
		setLayoutData(data);
	
	
	    final Table swtTable = new Table(this, SWT.BORDER | SWT.FULL_SELECTION | SWT.V_SCROLL | SWT.H_SCROLL | SWT.SINGLE );
		data = new GridData(GridData.FILL_HORIZONTAL | GridData.FILL_VERTICAL);
    
		TableLayout tableLayout = new TableLayout();
	    tableViewer =
		new TableViewer(swtTable);

	    swtTable.setLayout(tableLayout);
	    swtTable.setLayoutData(data);
	    swtTable.setHeaderVisible(true);
	    swtTable.setLinesVisible(true);

		tableLayout.addColumnData(new ColumnPixelData(150));
		TableColumn col = new TableColumn(swtTable, SWT.NONE);
		col.setText("Attribute name");
		
		tableLayout.addColumnData(new ColumnPixelData(250));
		col = new TableColumn(swtTable, SWT.NONE);
		col.setText("Value");

		tableViewer.setColumnProperties(new String[] {"Name","Value"}); //$NON-NLS-1$ //$NON-NLS-2$
		tableViewer.setUseHashlookup(true);
		tableViewer.setComparer(new IElementComparer() {

			public boolean equals(Object a, Object b) {
				return a == b;
			}

			public int hashCode(Object element) {
				return element == null ? 0 : element.hashCode();
			}
			
		});

		tableViewer.setCellModifier(
			new ICellModifier(){

				public boolean canModify(Object element, String property) {
					return true;
				}
	
				public Object getValue(Object element, String property) {
					AttributeDescriptorValue attrDescr = (AttributeDescriptorValue)element;
					context.setProperty("attributeName", attrDescr.getName()); //$NON-NLS-1$
					String tagName = "" + fWizardModel.getTagProposal().getName(); //$NON-NLS-1$
					String prefix = fWizardModel.getTagProposal().getPrefix();
					if(prefix != null && prefix.length() > 0 && !tagName.startsWith(prefix + ":")) { //$NON-NLS-1$ //$NON-NLS-2$
						tagName = prefix + ":" + tagName; //$NON-NLS-1$
					}
					context.setProperty("nodeName", tagName); //$NON-NLS-1$
					ISelection s = fWizardModel.getDropData().getSelectionProvider().getSelection();
					if (s instanceof TextSelection) {
						int offset = ((TextSelection)s).getOffset();
						context.put("offset", new Integer(offset));
					}
					return attrDescr.getValue()==null?"":attrDescr.getValue(); //$NON-NLS-1$
				}
	
				public void modify(Object element, String property, Object value) {
					TableItem item = (TableItem)element;
					AttributeDescriptorValue attrDescr = (AttributeDescriptorValue)item.getData();
					fWizardModel.setAttributeValue(attrDescr,value);
					if(tableViewer != null) {
						tableViewer.refresh();
					}
				}
			}
		);

		ExtendedCellEditorProvider provider = createCellEditorProvider();

		if(provider != null) {
			tableViewer.setCellEditors(
					new CellEditor[]{null,
							provider.createCellEditor(swtTable, context)}
			);
		} else {		
			tableViewer.setCellEditors(
					new CellEditor[]{null,
							new TextCellEditor(swtTable)}
			);
		}

		tableViewer.getCellEditors()[1].addListener(
			new ICellEditorListener() {

				public void applyEditorValue() {
				}

				public void cancelEditor() {
				}

				public void editorValueChanged(boolean oldValidState, boolean newValidState) {
					// TODO Auto-generated method stub
					if(tableViewer.isCellEditorActive()) {
						IStructuredSelection selection = (IStructuredSelection)tableViewer.getSelection();
						AttributeDescriptorValue attrValue = (AttributeDescriptorValue)selection.getFirstElement();
						fWizardModel.setAttributeValue(attrValue,tableViewer.getCellEditors()[1].getValue());
					}
				}
			}
		);
		tableViewer.setContentProvider(
			new IStructuredContentProvider() {

				public Object[] getElements(Object inputElement) {
					return filterElements();
				}

				private Object[] filterElements() {
  
					AttributeDescriptorValue[] value = fWizardModel.getAttributeValueDescriptors();
					if(!isFiltered()) return value;
					List<AttributeDescriptorValue> filtered = new ArrayList<AttributeDescriptorValue>();
					for (int i = 0; i < value.length; i++) {
						AttributeDescriptorValue value2 = value[i];
						if(value2.isRequired() || value2.isPreferable()) {
							filtered.add(value2);
						}
					}
					return filtered.toArray();
				}
				
				public void dispose() {
				}

				public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
				}
			}
		);

		tableViewer.setLabelProvider(
			new ITableLabelProvider () {
				
				public static final int ATTRIBUTE_NAME_INDEX = 0;
				public static final int ATTRIBUTE_VALUE_INDEX = 1;
				
				public Image getColumnImage(Object element, int columnIndex) {
					return null;
				}
	
				public String getColumnText(Object element, int columnIndex) {
					AttributeDescriptorValue attrDescr = (AttributeDescriptorValue)element;
					switch (columnIndex) {
						case ATTRIBUTE_NAME_INDEX:
							if(attrDescr.isRequired()) {
								return attrDescr.getName();
							} else if(attrDescr.isPreferable()) {
								return attrDescr.getName();								
							} else {
								return attrDescr.getName();								
							}
								
						case ATTRIBUTE_VALUE_INDEX:
							return attrDescr.getValue()==null?"":attrDescr.getValue().toString(); //$NON-NLS-1$
					}
					throw new IllegalArgumentException("Wrong column index for LabelProvider"); //$NON-NLS-1$
				}
	
				public void addListener(ILabelProviderListener listener) {
				}
	
				public void dispose() {
				}
	
				public boolean isLabelProperty(Object element, String property) {
					return false;
				}
	
				public void removeListener(ILabelProviderListener listener) {
				}
			}
		);
		tableViewer.setInput(new Object());
	}

	private Properties context = new Properties();
	
	private ExtendedCellEditorProvider createCellEditorProvider() {
		return (ExtendedCellEditorProvider)ModelFeatureFactory.getInstance().createFeatureInstance("org.jboss.tools.jst.jsp.outline.JSPCellEditorProviderImpl"); //$NON-NLS-1$
	}

	/**
	 * 
	 * @return
	 */
	protected boolean isFiltered() {
		return fFiltered ;
	}

	/**
	 * 
	 */
	public void propertyChange(PropertyChangeEvent evt) {
		if(tableViewer.isCellEditorActive()) return; 
		if(IDropWizardModel.TAG_PROPOSAL.equals(evt.getPropertyName())) {
			tableViewer.setInput(new Object());
			AttributeDescriptorValueProvider valueProvider = fWizardModel.getDropData().getValueProvider();
			if(valueProvider != null) {
				valueProvider.initContext(context);
			}
		} else {
			tableViewer.refresh();
		}
	}
	
	/**
	 * 
	 * @param attributes
	 * @return
	 */
	public static final boolean areThereAttributesForCommonTab(AttributeDescriptorValue[] attributes) {
		for (int i = 0; i < attributes.length; i++) {
			AttributeDescriptorValue value = attributes[i];
				if(value.isPreferable() || value.isRequired()) return true;
		}
		return false;
	}
	
	/**
	 * 
	 * @param model
	 */
	public void setWizardModel(IDropWizardModel model) {
		fWizardModel = model;
		tableViewer.setInput(new Object());
		tableViewer.refresh();
	}
	/**
	 * remove the selectin when switched
	 */
	public void widgetSelected(SelectionEvent e) {
		tableViewer.refresh();		
	}

	public void widgetDefaultSelected(SelectionEvent e) {
	}

}
