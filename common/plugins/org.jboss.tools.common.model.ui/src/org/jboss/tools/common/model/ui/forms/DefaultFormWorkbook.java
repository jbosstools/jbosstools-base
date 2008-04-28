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

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Vector;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;


public class DefaultFormWorkbook implements IFormWorkbook {
	private ArrayList initPages;
	private Hashtable pages;
//	private IFormPage sourcePage;
//	private IFormPage lastFormPage;
	private boolean firstPageSelected=true;
	private CTabFolder tabFolder;
	private Vector listeners=new Vector();
	private IFormPage currentPage;

	public DefaultFormWorkbook() {
		pages = new Hashtable();
		initPages = new ArrayList();
	}
	public void addFormSelectionListener(IFormSelectionListener listener) {
		listeners.addElement(listener);
	}
	public void addPage(IFormPage page) {
		if (tabFolder!=null) {
			CTabItem item = new CTabItem(tabFolder, SWT.NULL);
			item.setText(page.getLabel());
			item.setToolTipText(page.getTitle());
			item.setData(page);
			pages.put(page, item);
		} else {
			initPages.add(page);
		}
		// if (page.isSource()) sourcePage = page;
		
		if (tabFolder!=null) {
			if (firstPageSelected && currentPage == null)
				selectPage(page, true);
		}
	}
	public void createControl(Composite parent) {
		tabFolder = new CTabFolder(parent, SWT.BOTTOM);
		if (initPages.size()>0) {
			for (int i=0;i<initPages.size();++i) {
				IFormPage page = (IFormPage)initPages.get(i);
				CTabItem item = new CTabItem(tabFolder, SWT.NULL);
				item.setText(page.getLabel());
				item.setToolTipText(page.getTitle());
				item.setData(page);
				pages.put(page, item);

				// if (page.isSource()) sourcePage = page;

				if (firstPageSelected && currentPage == null)
					selectPage(page, true);
			}
		}
		
		tabFolder.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				CTabItem item = (CTabItem) e.item;
				IFormPage page = (IFormPage) item.getData();
				if (page != null)
					selectPage(page, true);
			}
		});
		// listener to resize visible components
		tabFolder.addListener(SWT.Resize, new Listener() {
			public void handleEvent(Event e) {
				if (currentPage != null)
					setControlSize(currentPage.getControl());
			}
		});
	}
	private void fireSelectionChanged(IFormPage page, boolean setFocus) {
		for (Iterator iter = listeners.iterator(); iter.hasNext();) {
			IFormSelectionListener listener = (IFormSelectionListener) iter.next();
			listener.formSelected(page, setFocus);
		}
	}
	public Control getControl() {
		return tabFolder;
	}
	public IFormPage getCurrentPage() {
		return currentPage;
	}
	public boolean isFirstPageSelected() {
		return firstPageSelected;
	}
	public void removeFormSelectionListener(IFormSelectionListener listener) {
		listeners.removeElement(listener);
	}
	public void removePage(IFormPage page) {
		CTabItem item = (CTabItem) pages.get(page);
		if (item != null)
			item.dispose();
	}
	private void reselectPage(final IFormPage page) {
		tabFolder.getDisplay().asyncExec(new Runnable() {
			public void run() {
				selectPage(page, true);
			}
		});
	}
	public void selectPage(final IFormPage page, final boolean setFocus) {
		final IFormPage oldPage = currentPage;
		currentPage = page;
	
		// It may take a while
		BusyIndicator.showWhile(tabFolder.getDisplay(), new Runnable() {
			public void run() {
				switchPages(oldPage, page, setFocus);
			}
		});
	}
	private void setControlSize(Control control) {
		Rectangle bounds = tabFolder.getBounds();
		Rectangle offset = tabFolder.getClientArea();
		bounds.x += offset.x;
		bounds.y += offset.y;
		bounds.width = offset.width;
		bounds.height = offset.height;
		control.setBounds(bounds);
		control.moveAbove(tabFolder);
	}
	private void setControlVisible(Control control) {
		if (control == null)
			return;
		setControlSize(control);
		control.setVisible(true);
	}
	public void setFirstPageSelected(boolean newFirstPageSelected) {
		firstPageSelected = newFirstPageSelected;
	}
	private void switchPages(IFormPage oldPage, IFormPage newPage, boolean setFocus) {
		if (oldPage != null && oldPage!=newPage) {
			boolean okToSwitch = oldPage.becomesInvisible(newPage);
			if (!okToSwitch) {
				// We must try to go back to the source page
				reselectPage(oldPage);
				return;
			}
		}
		if (newPage.getControl() == null)
			newPage.createControl(tabFolder);
		tabFolder.setSelection((CTabItem) pages.get(newPage));
		if (oldPage != null && oldPage != newPage) {
			Control oldControl = oldPage.getControl();
			if (oldControl!=null) oldControl.setVisible(false);
		}
		Control newControl = newPage.getControl();
		newPage.becomesVisible(oldPage);
		setControlVisible(newControl);
		fireSelectionChanged(newPage, setFocus);
	}
}
