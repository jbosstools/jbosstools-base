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
package org.jboss.tools.common.model.ui.forms;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.jboss.tools.common.editor.form.RightFormContainer;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.ModelUIMessages;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.attribute.XAttributeSupport;
import org.jboss.tools.common.model.ui.attribute.adapter.XChildrenTableStructuredAdapter;
import org.jboss.tools.common.model.ui.attribute.editor.ExtendedFieldEditor;
import org.jboss.tools.common.model.ui.attribute.editor.IFieldEditor;
import org.jboss.tools.common.model.ui.attribute.editor.IPropertyEditor;
import org.jboss.tools.common.model.ui.attribute.editor.IPropertyFieldEditor;
import org.jboss.tools.common.model.ui.attribute.editor.JavaEclipseChoicerEditor;
import org.jboss.tools.common.model.ui.attribute.editor.TableStructuredEditor;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

/**
 * @author Igels
 *
 */
public class Form extends ExpandableForm {
	
	private XAttributeSupport support;
	private XModelObject xmo;
//	private XModel model;

	private TableStructuredEditor tableEditor;
	private XChildrenTableStructuredAdapter tableAdapter;

//	private IPropertyEditor[] editors;
	private IFormData formData;
	private boolean table = false;
	private ArrayList<ExtendedFieldEditor> fieldEditors = new ArrayList<ExtendedFieldEditor>();
	
	private Form() {}

	protected XAttributeSupport getSupport() {
		return support;
	}

	protected XModelObject getModelObject() {
		return xmo;
	}

	public void dispose() {
		super.dispose();
		if (support!=null) support.dispose();
		support = null;
		if (tableEditor!=null) tableEditor.dispose();
		tableEditor = null;
		if (tableAdapter!=null) tableAdapter.dispose();
		tableAdapter = null;
		if (fieldEditors!=null) {
			Object object;
			Iterator i = fieldEditors.iterator();
			while (i.hasNext()) {
				object = i.next();
				if (object instanceof ExtendedFieldEditor) ((ExtendedFieldEditor)object).dispose();
			}
			fieldEditors.clear();
		}
		fieldEditors = null;
	}

	public Form(IFormData formData) {
		this.formData = formData;
		table = formData.getEntities()!=null;
		if(!table) {
///			editors = new IPropertyEditor[formData.getAttributes().length];
		}
		this.setHeadingText(formData.getHeader());
		support = new XAttributeSupport(formData.getWidgetSettings());
		this.setCollapsable(Boolean.TRUE.booleanValue());
	}

	protected Control createClientArea(Composite parent, IWidgetSettings settings) {
		Composite composite = new Composite(parent, SWT.NONE);
			composite.setBackgroundMode(SWT.INHERIT_DEFAULT);
		settings.setupControl(composite);
		GridLayout layout = new GridLayout(2, Boolean.FALSE.booleanValue());

		layout.horizontalSpacing = 5;
		layout.verticalSpacing = 5;
		layout.marginHeight = 5;
		layout.marginWidth = 5;
		composite.setLayout(layout);
		Control[] control = null;
		GridData gd;

		String description = formData.getDescription();
		if(description!=null && description.length()>0) {
			Label label = new Label(composite, SWT.WRAP);
			label.setText(description);
			settings.setupControl(label);
			gd = new GridData();
			gd.horizontalSpan = 2;
			label.setLayoutData(gd);
		}
		
		if (xmo==null) return composite;

		IFormAttributeData[] attributes = formData.getAttributes();
		if(!table) {
			for(int i=0; i<attributes.length; i++) {
				IPropertyEditor editor = support.getPropertyEditorByName(attributes[i].getName());
				if(editor != null) {
					if(attributes[i].getWraperClassName()!=null 
							&& !(editor instanceof JavaEclipseChoicerEditor)) {
						try {
							Class wraperClass = Class.forName(attributes[i].getWraperClassName());
							Constructor wraperConstructor = wraperClass.getConstructor(new Class[]{IWidgetSettings.class});
							IPropertyFieldEditor wraper = (IPropertyFieldEditor)wraperConstructor.newInstance(new Object[]{formData.getWidgetSettings()});

							((ExtendedFieldEditor)wraper).setLabelText(editor.getLabelText());
							wraper.setPropertyEditor(editor);
							((ExtendedFieldEditor)wraper).fillIntoGrid(composite, 2);
							((ExtendedFieldEditor)wraper).setEnabled(xmo.isAttributeEditable(attributes[i].getName()));
							fieldEditors.add((ExtendedFieldEditor)wraper);
							support.registerFieldEditor(editor.getAttributeName(), (ExtendedFieldEditor)wraper);
						} catch (ClassNotFoundException e) {
							ModelUIPlugin.getPluginLog().logError(e);
						} catch (NoSuchMethodException e) {
							ModelUIPlugin.getPluginLog().logError(e);
						} catch (InstantiationException e) {
							ModelUIPlugin.getPluginLog().logError(e);
						} catch (IllegalAccessException e) {
							ModelUIPlugin.getPluginLog().logError(e);
						} catch (InvocationTargetException e) {
							ModelUIPlugin.getPluginLog().logError(e);
						}
					} else {
						ExtendedFieldEditor fieldEditor = (ExtendedFieldEditor)((IFieldEditor)editor.getFieldEditor(composite));
						fieldEditors.add(fieldEditor);
						control = ((IFieldEditor)fieldEditor).getControls(composite);
						control[0].setLayoutData(attributes[i].getLayoutDataFactory().createLayoutData(AttributeControlType.LABEL));
						control[1].setLayoutData(attributes[i].getLayoutDataFactory().createLayoutData(AttributeControlType.EDITOR));
						fieldEditor.setEnabled(xmo.isAttributeEditable(attributes[i].getName()));
						support.registerFieldEditor(editor.getAttributeName(), (ExtendedFieldEditor)fieldEditor);
					}
				} else {
					ModelUIPlugin.getPluginLog().logInfo( NLS.bind(ModelUIMessages.Form_CANNOT_LOAD_ATTRIBUTE, attributes[i].getName()));
				}
            }
		} else {
			ExtendedFieldEditor fieldEditor = (ExtendedFieldEditor)((IFieldEditor)tableEditor.getFieldEditor(composite)); 
			fieldEditors.add(fieldEditor);
			control = ((IFieldEditor)fieldEditor).getControls(composite);

			control[0].dispose(); // cannot show label

			gd = new GridData(GridData.FILL_BOTH);
			gd.horizontalSpan = 2;
			control[1].setLayoutData(gd);
			fieldEditor.setEnabled(xmo.isObjectEditable());
		}

		return composite;
	}

	public void initialize(Object model) {
		this.xmo = (XModelObject)model;
		if(xmo == null) {
			ModelUIPlugin.getPluginLog().logInfo( "Error to create form "+formData.getHeader()+". Model object cannot be null.", new IllegalArgumentException("Parameter cannot be null")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			return;
		}
		this.model = xmo.getModel();
		this.support.init(xmo);
		this.support.setAutoStore(Boolean.TRUE.booleanValue());

		IFormAttributeData[] attributes = formData.getAttributes();
		IFormActionData[] actions = formData.getActions();
		String[] entities = formData.getEntities();
		if(!table) {

		} else {
			this.tableAdapter = new XChildrenTableStructuredAdapter();
			this.tableAdapter.setShownEntities(entities/*new String[]{entities[0]}*/);
			this.tableAdapter.getActionMapping().clear();

			for(int i=0; i<actions.length; i++) {
				this.tableAdapter.getActionMapping().put(actions[i].getActionLabel(), actions[i].getActionPath());
			}

			String[] shownProperties = new String[attributes.length];
			String[] columnLabels = new String[attributes.length];
			int[] widths = new int[attributes.length];
			for(int i=0; i<attributes.length; i++) {
				shownProperties[i] = attributes[i].getName();
				columnLabels[i] = attributes[i].getDisplayName();
				widths[i] = attributes[i].getWidth();
			}

			this.tableAdapter.setShownProperties(shownProperties);
			this.tableAdapter.setColumnLabels(columnLabels);
			this.tableAdapter.setWidths(widths);
			this.tableAdapter.setModelObject(xmo);

			this.tableEditor = new TableStructuredEditor(formData.getWidgetSettings());
			this.tableEditor.setLabelText(""); //$NON-NLS-1$
			this.tableEditor.setInput(this.tableAdapter); 
		}

	}
	
	private ISelectionChangedListener findSelectionChangedListener(IFormContainer container) {
		if (container==null) {
			return null;
		} 
		if (container instanceof RightFormContainer) {
			return ((RightFormContainer)container).getSelectionChangedListener();
		}
		return findSelectionChangedListener(container.getParent());
	}
	
	public void setParent(IFormContainer container) {
		super.setParent(container);
		if (this.tableAdapter!=null) {
			this.tableAdapter.setSelectionChangedListener(findSelectionChangedListener(container));
		}
	}

	
	public void update() {
		if (support != null) {
			support.load();			
			support.updateEnablementByModelObject();
		}
	}

	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		/*
		if (this.support!=null) {
			support.setEnabled(enabled);
		}
		if (fieldEditors!=null && fieldEditors.size()>0) {
			Iterator i = fieldEditors.iterator();
			while (i.hasNext()) ((ExtendedFieldEditor)i.next()).setEnabled(enabled);
		}
		*/
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.forms.IForm#doGlobalAction(java.lang.String)
	 */
	public boolean doGlobalAction(String actionId) {
		return support.doGlobalAction(actionId);
	}
}