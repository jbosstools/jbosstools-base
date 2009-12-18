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
package org.jboss.tools.common.model.ui.objecteditor;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.viewers.ColumnLayoutData;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class XTable {
	protected XTableProvider provider;
	protected XTableImageProvider imageProvider;
	protected int style = SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER;
	protected boolean autoResizeColumns = false; 
	protected boolean lock = false;
	protected TableViewer tv;
	protected boolean isHeaderVisible = true;
	
	public void dispose() {
		if (provider!=null) provider.dispose();
		provider = null;
		if (imageProvider!=null) imageProvider.dispose();
		imageProvider = null;
		tv = null; 
	}
	
	public void setAutoResize(boolean b) {
		autoResizeColumns = b;
	}

	public void setTableProvider(XTableProvider provider) {
		this.provider = provider;
		if(provider instanceof XTableImageProvider) {
			imageProvider = (XTableImageProvider)provider;
		}
	}
	
	public TableViewer getViewer() {
		return tv;
	}
	
	public void setMultiSelected() {
		style |= SWT.MULTI;
	}

	public Control createControl(Composite parent) {
		return createControl(parent, style);
	}
	
	public Control createControl(Composite parent, int style) {
		this.style = style;
		Table table = new Table(parent, style);
		int k = provider.getColumnCount();
		TableLayout layout = new TableLayout();
		table.setLayout(layout);	
		
		int prefferedColumnWidth = 0;
		for (int i = 0; i < k; i++) {
			prefferedColumnWidth += provider.getWidthHint(i);
		}
		widths = new int[k];
		for (int i = 0; i < k; i++) {
			widths[i] = provider.getWidthHint(i)*100/prefferedColumnWidth;
		}
		ColumnLayoutData layoutData;
		for (int i = 0; i < k; i++) {
			TableColumn c = new TableColumn(table, SWT.NONE);
			c.setText(provider.getColumnName(i));
			layoutData = new ColumnWeightData(widths[i], true);
			layout.addColumnData(layoutData);
		}
		table.setHeaderVisible(isHeaderVisible);
		table.setLinesVisible(true);
		tv = new TableViewer(table);

		if(k < 3 || autoResizeColumns) {
			table.addControlListener(new Resize(table));
		}
 
		return table;	
	}

	public void updateLayout() {
		if(resizeLock) return;
		Table table = getTable();
		if(table == null || table.isDisposed()) return;
		resizeLock = true;
		try {
			updateLayoutInternal(table);
		} finally {
			resizeLock = false;
		}
	}

	private void updateLayoutInternal(Table table) {
		int k = provider.getColumnCount();
		int[] newWidth = new int[k];
		int delta = 0;
		if(table.isVisible()) {
 			int w = table.getClientArea().width - 1;
 			int cw = 0, hs = 0;
 			for (int i = 0; i < table.getColumnCount(); i++) {
				cw += table.getColumn(i).getWidth();
				hs += getWidthHint(i);
			}				 
			for (int i = 0; i < table.getColumnCount(); i++) {
				TableColumn c = table.getColumn(i);
				int dw = (w - cw) * getWidthHint(i) / hs;
				newWidth[i] = c.getWidth() + dw;
				delta += Math.abs(dw); 
			}
			if(delta < 5) newWidth = widths;
		} else {
			newWidth = widths;
		}
		for (int i = 0; i < k; i++) {
			if(newWidth[i] < 0) return;
		}
		TableLayout layout = new TableLayout();
		for (int i = 0; i < k; i++) {
			layout.addColumnData(new ColumnWeightData(newWidth[i], true));
		}
		widths = newWidth;
		table.setLayout(layout);
		table.layout();
	}
	
	int[] widths = null;

	protected int getWidthHint(int i) {
		return (widths == null || widths.length < 2 || widths[i] == 0) ? 10 : widths[i];
	}
		
	public Control getControl() {
		return getTable();
	}
	
	public Table getTable() {
		return (tv == null) ? null : tv.getTable();
	}
	
	public boolean isActive() {
		Table t = getTable();
		return t != null && !t.isDisposed(); 
	}
	
	private boolean resizeLock = false;

	public synchronized void update() {
		Table table = getTable();
		if(table == null || table.isDisposed()) return;
		int r = table.getSelectionIndex();
		TableItem item = null;
		resizeLock = true;
		for (int i = 0; i < provider.getRowCount(); i++) {
			if(table.getItemCount() > i) {
			    item = table.getItem(i);
			} else {
				item = new TableItem(table, SWT.BORDER, i);
			}
			String[] vs = new String[table.getColumnCount()];
			for (int j = 0; j < vs.length; j++) {
				vs[j] = toVisualValue(provider.getValueAt(i, j));
			}
			item.setText(vs);
			if(imageProvider != null) {
				item.setImage(imageProvider.getImage(i));
			}
			Object data = provider.getDataAt(i);
			if(data != null) item.setData(data);
			Color color = provider.getColor(i);
			if(color != null) item.setForeground(color);
		}
		if(table.getTopIndex() > provider.getRowCount()) {
			int ti = provider.getRowCount() - 20;
			if(ti < 0) ti = 0;
			table.setTopIndex(ti);
		}
		table.remove(provider.getRowCount(), table.getItemCount()- 1);
		if(r >= 0) try {
			table.setSelection(r);
		} catch (SWTException e) {
			//ignore
		}
		resizeLock = false;
	}
	
	public static String toVisualValue(String v) {
		if(v == null) return ""; //$NON-NLS-1$
		if(v.indexOf('\n') >= 0) v = v.replace('\n', ' ');
		if(v.indexOf('\t') >= 0) v = v.replace('\t', ' ');
		if(v.indexOf('\r') >= 0) v = v.replace('\r', ' ');
		return v;
	}
	
	public int getSelectionIndex() {
		return getTable().getSelectionIndex();
	}
	
	public void setSelection(int i) {
		getTable().setSelection(i);
	}

	protected Set<String> getKeys() {
		Set<String> set = new HashSet<String>();
		Table table = getTable();
		int c = (table == null || table.isDisposed()) ? 0 : table.getItemCount();		
		for (int i = 0; i < c; i++) 
		  set.add(getTable().getItem(i).getText(0));
		return set;
	}

	protected int getAddedKey(Set set) {
		Table table = getTable();
		if(table == null || table.isDisposed()) return -1;
		int c = table.getItemCount();		
		for (int i = 0; i < c; i++)
		  if(!set.contains(getTable().getItem(i).getText(0))) return i;
		return -1;
	}
	
	class Resize extends ControlAdapter {
		Resize(Table table) {
			table.addControlListener(this);
		}
		public void controlResized(ControlEvent e) {
			updateColumnWidth();
		}
		private void updateColumnWidth() {
			updateLayout();
		}
	}
	
	public void setHeaderVisible(boolean b) {
		if(isHeaderVisible == b) return;
		isHeaderVisible = b;
		if(isActive()) getTable().setHeaderVisible(isHeaderVisible);
	}

}
