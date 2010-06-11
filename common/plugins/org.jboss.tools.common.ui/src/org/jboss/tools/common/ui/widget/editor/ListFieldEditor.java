/******************************************************************************* 
 * Copyright (c) 2010 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/

package org.jboss.tools.common.ui.widget.editor;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.ui.dialogs.FilteredItemsSelectionDialog;

/**
 * This editor allows to add to a list several items invoking by 'Add' button a selection dialog
 * derived from FilteredItemsSelectionDialog.
 * If ListFieldEditorProvider is not set, selection dialog will have nothing to pick up. 
 * 
 * @author Viacheslav Kabanovich
 */
public class ListFieldEditor extends BaseListFieldEditor {
	
	public static interface ListFieldEditorProvider<T> {

		public FilteredItemsSelectionDialog createSelectionDialog();
		
		public T getSelected(Object selected);

		public T[] getSelectableObjects();
		
		public ILabelProvider createLabelProvider();
	}

	ListFieldEditorProvider<?> provider = null;

	public ListFieldEditor(String name, String label, Object defaultValue) {
		super(name, label, defaultValue);
	}

	public void setProvider(ListFieldEditorProvider<?> provider) {
		this.provider = provider;
	}

	protected ILabelProvider createLabelProvider() {
		if(provider != null) {
			return provider.createLabelProvider();
		}
		return new LabelProvider();
	}

	protected List<Object> runAddAction() {
		List<Object> added = new ArrayList<Object>();
		if(provider != null) {
			FilteredItemsSelectionDialog dialog = provider.createSelectionDialog();
			int result = dialog.open();
			if(result == FilteredItemsSelectionDialog.OK) {
				Object[] os = dialog.getResult();
				if(os != null) {
					for (Object o: os) {
						Object v = provider.getSelected(o);
						if(v != null) {
							added.add(v);
						}
					}
				}
			}
		}
		return added;
	}

}
