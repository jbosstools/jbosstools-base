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

import java.text.MessageFormat;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.internal.ui.dialogs.FilteredTypesSelectionDialog;
import org.eclipse.jdt.ui.dialogs.ITypeInfoFilterExtension;
import org.eclipse.jdt.ui.dialogs.ITypeInfoRequestor;
import org.eclipse.jdt.ui.dialogs.TypeSelectionExtension;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.FilteredItemsSelectionDialog;

import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.attribute.IValueFilter;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class JavaEclipseChoicerEditor extends ValueEditor {

	protected JavaHyperlinkCellEditor cellEditor;
	//protected JavaChoicerFieldEditor fieldEditor;
	protected JavaHyperlinkLineFieldEditor fieldEditor;

	public JavaEclipseChoicerEditor() {}

	public JavaEclipseChoicerEditor(IWidgetSettings settings) {
		super(settings);
	}

	public void dispose() {
		super.dispose();
		if (cellEditor!=null) cellEditor.dispose();
		cellEditor = null;
		if (fieldEditor!=null) fieldEditor.dispose();
		fieldEditor = null;
	}

	public boolean isGreedyEditor() {
		return false;
	}

	protected CellEditor createCellEditor(Composite parent) {
//		cellEditor = new DialogCellEditorEx(parent, SWT.NONE);
		cellEditor = new JavaHyperlinkCellEditor(parent, SWT.NONE);		
		cellEditor.setPropertyEditor(this);
		return cellEditor;
	}

	protected ExtendedFieldEditor createFieldEditor(Composite parent) {
		fieldEditor = new JavaHyperlinkLineFieldEditor(settings);
		return fieldEditor;
	}
	
	public String getChangeButtonName() {
		return JFaceResources.getString("openBrowse"); //$NON-NLS-1$
	}

	public boolean callsExternal() {
		return true;
	}

	public Object callExternal(Shell shell) {
		IJavaProject jp = null;
		DefaultValueAdapter adapter = (DefaultValueAdapter)getInput();
		XModelObject o = adapter.getModelObject();
		if(o != null) {
			IProject p = EclipseResourceUtil.getProject(o);
			if(p != null) {
				jp = EclipseResourceUtil.getJavaProject(p);
			}
		}
		String title = MessageFormat.format("Select {0}", getAttributeName());
		if(adapter != null && adapter.getAttribute() != null) {
			String key = "" + adapter.getAttribute().getModelEntity().getName() + "." + adapter.getAttribute().getName().replace(' ', '_') + ".edit"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			String t = WizardKeys.getLabelText(key);
			if(t != null) {
				title = t;
			} else {
				title = MessageFormat.format("Select {0}", 
						WizardKeys.getAttributeDisplayName(adapter.getAttribute(), true));
			}
		}
		
	FilteredTypesSelectionDialog dialog = new FilteredTypesSelectionDialog(
			shell, 
			false, 
			ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow(),
			jp == null ? SearchEngine.createWorkspaceScope() 
					   : SearchEngine.createJavaSearchScope(new IJavaElement[]{jp}), 
			IJavaSearchConstants.TYPE, createTypeSelectionExtension());
	dialog.setTitle(title);
	IValueProvider valueProvider = (IValueProvider)adapter.getAdapter(IValueProvider.class);
	String v = valueProvider.getStringValue(true);
	dialog.setInitialPattern(v);
	int status = dialog.open();
	if(status == FilteredItemsSelectionDialog.OK) {
		Object result = dialog.getFirstResult();
		if(result instanceof IType) {
			return ((IType)result).getFullyQualifiedName('.');
		}
	}
	return null;
	}
	
	TypeSelectionExtension createTypeSelectionExtension() {
		final ITypeInfoFilterExtension filter = createFilterExtension();
		if(filter == null) return null;
		return new TypeSelectionExtension() {
			public ITypeInfoFilterExtension getFilterExtension() {
				return filter;
			}
		};
	}
	
	ITypeInfoFilterExtension createFilterExtension() {
		if(getInput() instanceof IAdaptable) {
			IValueFilter filter = (IValueFilter)((IAdaptable)getInput()).getAdapter(IValueFilter.class);
			if(filter != null) {
				return new FilterExtension(filter);
			}
		}		
		return null;
	}
	
	class FilterExtension implements ITypeInfoFilterExtension {
		IValueFilter filter;
		public FilterExtension(IValueFilter filter) {
			this.filter = filter;
		}
		public boolean select(ITypeInfoRequestor typeInfoRequestor) {
			String pkg = typeInfoRequestor.getPackageName();
			String cls = typeInfoRequestor.getTypeName();
			String q = pkg == null || pkg.length() == 0 ? cls : pkg + "." + cls;
			return filter.accept(q);
		}		
	}
}
