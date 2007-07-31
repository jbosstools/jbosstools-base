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
package org.jboss.tools.common.model.ui.attribute.editor;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

import org.jboss.tools.common.model.ui.IListEditor;
import org.jboss.tools.common.model.ui.IStructuredChangeListener;
import org.jboss.tools.common.model.ui.IStructuredEditor;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.StructuredChangedEvent;
import org.jboss.tools.common.model.ui.attribute.IListContentProvider;
import org.jboss.tools.common.model.ui.attribute.adapter.ITableAdapter;
import org.jboss.tools.common.model.ui.attribute.adapter.StructuredListAdapter.INewValueProvider;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import org.jboss.tools.common.model.ui.actions.IActionProvider;
import org.jboss.tools.common.model.ui.widgets.BorderedControl;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.ui.widgets.ScrolledComposite;
import org.jboss.tools.common.model.ui.widgets.border.Border;

public class TableStructuredFieldEditor extends ExtendedFieldEditor 
	implements IPropertyFieldEditor, IStructuredEditor, IListEditor, IFieldEditor, PropertyChangeListener, IStructuredChangeListener, ISelectionChangedListener {
	// controls
	// label get from superclass
	private TableViewer tableViewer;
	private Control tableControl;
	private Table tableField;
	private IPropertyEditor propertyEditor;
	
	//following fields are never read
	IStructuredContentProvider structuredContentProvider;
	IStructuredChangeListener structuredChangeListener;
	INewValueProvider newValueProvider;
	ILabelProvider labelProvider;
	IListContentProvider listContentProvider;
	ITableAdapter tableAdapter;
	Composite composite;
	Composite tableButtonsControl;
	PropertyChangeSupport pcs = new PropertyChangeSupport(this);
	
	public TableStructuredFieldEditor() {}

	public TableStructuredFieldEditor(IWidgetSettings settings) {
		super(settings);
	}
	
	protected void adjustForNumColumns(int numColumns) {}
	protected void doFillIntoGrid(Composite parent, int numColumns) {}
	protected void doLoad() {}
	protected void doLoadDefault() {}
	protected void doStore() {}
	
	public int getNumberOfControls() {
		return 2;
	}

	// IPropertyFieldEditor
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		initialize();
	}

	// IStructuredEditor
	public void setStructuredContentProvider(IStructuredContentProvider structuredContentProvider) {
		this.structuredContentProvider = structuredContentProvider;
	}

	// IStructuredEditor
	public void setStructuredChangeListener(IStructuredChangeListener structuredChangeListener) {
		this.structuredChangeListener = structuredChangeListener;
	}

	// IStructuredEditor
	public void setNewStructuredElementProvider(INewValueProvider newValueProvider) {
		this.newValueProvider = newValueProvider;
	}

	// IListEditor
	public void setLabelProvider(ILabelProvider labelProvider) {
		this.labelProvider = labelProvider;
	}

	// IListEditor
	public void setListContentProvider(IListContentProvider listContentProvider) {
		this.listContentProvider = listContentProvider;
	}

	// IFieldEditor
	public Control[] getControls(Composite parent) {
		return new Control[] {getLabelComposite(parent), createTableButtonsComponent(parent)};
	}

	// PropertyChangeListener
	// listen change value from adapter
	public void propertyChange(PropertyChangeEvent evt) {
	}

	protected void applyButtonSkin(Control button) {
		if (button!=null) {
			Color bg = getSettings().getColor("Button.Background");
			Color fg = getSettings().getColor("Button.Foreground");
			Font font = getSettings().getFont("Button.Font");

			button.setBackground(bg);
			button.setForeground(fg);
			button.setFont(font);
		}
		
	}
	
	private IActionProvider actionProvider;
	private ActionButtonControl actionButtonControl;
	
	public Control createButtonsControl(Composite parent) {
		return actionButtonControl.createControl(parent);
	}

	private ActionButtonControl createActionButtonControl() {
		ActionButtonControl actionButtonControl = new ActionButtonControl();
		actionButtonControl.setPropertyEditor(propertyEditor);
		return actionButtonControl;
	}

	public Control getTableControl() {
		return tableControl;
	}

	public Control createTableControl(Composite parent) {
		if (tableField == null) {
			int style = getSettings().getStyle("Table.Style");
			Color bg = getSettings().getColor("Table.Background");
			Color fg = getSettings().getColor("Table.Foreground");
			Font font = getSettings().getFont("Table.Font");
			Border border = getSettings().getBorder("Table.Border");
			if (style==SWT.DEFAULT) style = SWT.NONE;
			
			if (border!=null) {
				BorderedControl borderedControl = new BorderedControl(parent, SWT.NONE, border);
				tableField = new Table(borderedControl, style | SWT.FULL_SELECTION);
				tableControl = borderedControl;
			} else {
				tableField = new Table(parent, SWT.BORDER | SWT.FULL_SELECTION);
				tableControl = tableField;
			}
			tableField.setFont(font);
			tableField.setBackground(bg);
			tableField.setForeground(fg);
			tableField.setHeaderVisible(true);
			tableField.setLinesVisible(true);
			tableField.setLayout(new TableLayout());
			tableField.addMouseListener(new MouseListener() {

				public void mouseDoubleClick(MouseEvent e) {
					if(getActionProvider() == null) return;
					Display.getDefault().asyncExec(new Runnable() {
						public void run() {
							IAction action = getActionProvider().getAction(TableStructuredEditor.DOUBLE_CLICK__ACTION);
							if (action!=null) action.run();
						}
					});
				}

				public void mouseDown(MouseEvent e) {
				}

				public void mouseUp(MouseEvent e) {
				}
				
			});
			
			// create columns
			if (tableAdapter!=null) {
				TableColumn[] cs = tableAdapter.createTableColumn(tableField, SWT.NONE);
				if(cs != null && cs.length == 1) {
					tableField.setLinesVisible(false);
					tableField.setHeaderVisible(false);
					GridData g = new GridData(GridData.FILL_BOTH);
					g.horizontalSpan = 2;
				}
			}
			
			// create TableViewer
			if (tableViewer==null) {
				tableViewer = new TableViewer(tableField);
			}
			if (tableAdapter!=null) {
				tableViewer.setLabelProvider(tableAdapter.getTableLabelProvider());
			}
			if (tableAdapter!=null) {
				tableViewer.setContentProvider(tableAdapter);
			}
			if (tableAdapter!=null) {
				tableViewer.setInput(this);
			}
			if (tableAdapter!=null) {
				tableViewer.addSelectionChangedListener(tableAdapter);
				tableViewer.addSelectionChangedListener(new ISelectionChangedListener() {
					public void selectionChanged(SelectionChangedEvent event) {
						if(tableField == null || tableField.isDisposed() || !tableField.isFocusControl()) return;
						TableItem[] items = tableField.getSelection();
						if(items == null || items.length == 0) return;
						Rectangle r = items[0].getBounds(0);
						ScrolledComposite.scrollToVisible(tableField, r);
					}
				});
				tableField.addFocusListener(new FocusListener() {
					public void focusGained(FocusEvent e) {
						if(tableField == null || tableField.isDisposed() || !tableField.isFocusControl()) return;
						TableItem[] items = tableField.getSelection();
						if(items != null && items.length > 0) {
							Rectangle r = items[0].getBounds(0);
							ScrolledComposite.scrollToVisible(tableField, r);
						} else {
							ScrolledComposite.scrollToVisible(tableField, new Rectangle(0, 0, 100, 10));
						}
					}
					public void focusLost(FocusEvent e) {
					}
				});
			}
			try {
				structureChanged(null);
			} catch (Exception e ) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
			
		}
		return this.tableControl;
		
	}
	
	public Control createTableButtonsComponent(Composite parent) {
		if (tableButtonsControl==null) {
			tableButtonsControl = new Composite(parent, SWT.NONE);
			tableButtonsControl.setBackground(parent.getBackground());
			
			GridData gd;
			GridLayout layout = new GridLayout(3, Boolean.FALSE.booleanValue());
			layout.horizontalSpacing = 0;
			layout.verticalSpacing = 0;
			layout.marginHeight = 0;
			layout.marginWidth = 0;
			tableButtonsControl.setLayout(layout);
			
			Label l = new Label(tableButtonsControl, SWT.NONE);
			gd = new GridData(GridData.FILL_HORIZONTAL);
			gd.horizontalSpan = 3;
			l.setLayoutData(gd);
			
			Control control;
			// table
			control = createTableControl(tableButtonsControl);
			if(tableField.getColumnCount() == 1) {
				l.setText("" + tableField.getColumn(0).getText());
			} else {
				l.dispose();
			}
			gd = new GridData(GridData.FILL_BOTH);
			control.setLayoutData(gd);
			if (this.tableAdapter.getActionProvider()!=null && 
			        this.tableAdapter.getActionProvider().getActions()!=null && 
			        this.tableAdapter.getActionProvider().getActions().length>0) {
				// divider
				control = new Composite(tableButtonsControl, SWT.NONE);
				control.setBackground(tableButtonsControl.getBackground());
				gd = new GridData();
				gd.widthHint = 5;
				control.setLayoutData(gd);
				// buttons
				control = createButtonsControl(tableButtonsControl);
				if (control!=null) {
					gd = new GridData();
					gd.verticalAlignment = SWT.UP;
					control.setLayoutData(gd);
				}
			}
		}
		return tableButtonsControl;
	}
	
	public Control getControl() {
		return this.tableControl;
	}
	
	private void initialize() {
		if (propertyEditor!=null) {
			this.tableAdapter = (ITableAdapter)propertyEditor.getAdapter(ITableAdapter.class);
		}
		if (tableAdapter!=null) {
			tableAdapter.addStructureChangeListener(this);
			tableAdapter.addSelectionChangedListener(this);

			actionProvider = (IActionProvider)propertyEditor.getAdapter(IActionProvider.class);

			actionButtonControl = createActionButtonControl();
			actionButtonControl.setPropertyEditor(propertyEditor);
			actionButtonControl.setSettings(getSettings());
		}
	}

	// IStructureChangeListener
	public void structureChanged(StructuredChangedEvent event) {
		if (this.tableViewer!=null) {
			try {
				int i = tableViewer.getTable().getSelectionIndex();
				if(i < 0) i = 0;
				tableViewer.refresh();
				int c = tableViewer.getTable().getItemCount();
				while(i >= c) --i;
				if(i >= 0) {
					tableViewer.setSelection(new StructuredSelection(tableViewer.getTable().getItem(i).getData()));
				}
			} catch (Exception e) {}
		}
	}
	
	//ISelectionChangedListener
	public void selectionChanged(SelectionChangedEvent event) {
		tableAdapter.removeSelectionChangedListener(this);
		tableViewer.setSelection(event.getSelection());
		tableAdapter.addSelectionChangedListener(this);
		if(actionProvider != null) {
			actionProvider.update(event.getSelection());
		}
	}

	protected IActionProvider getActionProvider() {
		return actionProvider;
	}

	protected void setActionProvider(IActionProvider provider) {
		actionProvider = provider;
	}

	public void setEnabled(boolean enabled){
		super.setEnabled(enabled);
		//if (this.getTableControl()!=null) getTableControl().setEnabled(enabled);
		// it must make model in action provider // if (actionButtonControl!=null) actionButtonControl.setEnabled(enabled);
	}

    public TableViewer getTableViewer() {
        return tableViewer;
    }

	public void cut() {}

	public void copy() {}

	public void paste() {}

	public void delete() {}

}
