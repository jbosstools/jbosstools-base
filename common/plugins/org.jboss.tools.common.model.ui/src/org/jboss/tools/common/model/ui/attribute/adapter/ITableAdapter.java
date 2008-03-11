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

import org.jboss.tools.common.model.ui.IStructuredChangeListener;
import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

import org.jboss.tools.common.model.ui.actions.IActionProvider;

//public interface ITableAdapter extends IStructuredContentProvider, ITableLabelProvider, IColorProvider, ISelectionChangedListener, PropertyChangeListener, ISelectionProvider {
public interface ITableAdapter extends IStructuredContentProvider, IColorProvider, ISelectionChangedListener, ISelectionProvider {
	public IActionProvider getActionProvider();
	public TableColumn[] createTableColumn(Table table, int style);
	//public IButtonControl getButtonControl();
	
	public void addStructureChangeListener(IStructuredChangeListener listener);
	public void removeStructureChangeListener(IStructuredChangeListener listener);
	
	public ITableLabelProvider getTableLabelProvider();
	public void setTableLabelProvider(ITableLabelProvider tableLabelProvider);
}
