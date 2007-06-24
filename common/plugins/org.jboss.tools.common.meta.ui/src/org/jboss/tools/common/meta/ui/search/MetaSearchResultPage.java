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
package org.jboss.tools.common.meta.ui.search;

import org.jboss.tools.common.model.ui.navigator.NavigatorLabelProvider;
import org.eclipse.jface.viewers.*;
import org.eclipse.search.ui.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.IPageSite;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;
import org.jboss.tools.common.model.util.FindObjectHelper;

public class MetaSearchResultPage implements ISearchResultPage, ISearchResultListener {
	ISearchResultViewPart part;
	MetaSearchResult search;
	Object uiState;
	String id;
	IPageSite site;
	TreeViewer viewer;

	public Object getUIState() {
		return uiState;
	}

	public void setInput(ISearchResult search, Object uiState) {
		this.search = (MetaSearchResult)search;
		if(search == null) return;
		search.removeListener(this);
		search.addListener(this);
		this.uiState = uiState;
		if(viewer != null) {
			try {
				viewer.refresh();
			} catch (Exception e) {
				//ignore
			}
		}
	}

	public void setViewPart(ISearchResultViewPart part) {
		this.part = part;
	}

	public void restoreState(IMemento memento) {
	}

	public void saveState(IMemento memento) {
	}

	public void setID(String id) {
		this.id = id;
	}

	public String getID() {
		return id;
	}

	public String getLabel() {
		if(search != null) {
			ISearchQuery query = search.getQuery();
			if(query instanceof MetaSearchQuery) {
				MetaSearchQuery q = (MetaSearchQuery)query;
				return "\"" + q.getTextToFind() + "\"" + " - " + search.getObjects().size() + " matches"; 
			}
		}
		return "Meta Search";
	}

	public IPageSite getSite() {
		return site;
	}

	public void init(IPageSite site) throws PartInitException {
		this.site = site;
	}

	public void createControl(Composite parent) {
		viewer = new TreeViewer(parent);
		viewer.setUseHashlookup(false);
		viewer.getControl().setLayoutData(new GridData(GridData.FILL_BOTH));
		viewer.setContentProvider(new ContentProvider());
		viewer.setLabelProvider(new MetaLabelProvider());
		viewer.setInput(new Object());
		viewer.addDoubleClickListener(new IDoubleClickListener() {
			public void doubleClick(DoubleClickEvent event) {
				ISelection s = event.getSelection();
				if(s.isEmpty() || !(s instanceof StructuredSelection)) return;
				StructuredSelection ss = (StructuredSelection)s;
				XModelObject o = (XModelObject)ss.getFirstElement();
				FindObjectHelper.findModelObject(o, FindObjectHelper.EVERY_WHERE);
			}
		
		}
		);
		
	}

	public void dispose() {
		if(viewer != null) {
			if(viewer.getControl() != null && !viewer.getControl().isDisposed()) viewer.getControl().dispose();
			site = null;
			search.removeListener(this);
			search = null;
			viewer = null;
		}
	}

	public Control getControl() {
		return viewer == null ? null : viewer.getControl();
	}

	public void setActionBars(IActionBars actionBars) {
	}

	public void setFocus() {
	}
	
	class ContentProvider implements ITreeContentProvider {

		public Object[] getElements(Object inputElement) {
			if(search == null) return new Object[0];
			return search.getObjects().toArray(new Object[0]);
		}

		public void dispose() {}

		public void inputChanged(final Viewer viewer, Object oldInput, final Object newInput) {
			Display.getDefault().asyncExec(new Runnable() {
				public void run() {
					if(viewer == null || newInput == null) return;
					try { viewer.refresh(); } catch (Exception e) {}
				}
			});
		}

		public Object[] getChildren(Object parentElement) {
			return new Object[0];
		}

		public Object getParent(Object element) {
			return null;
		}

		public boolean hasChildren(Object element) {
			return false;
		}
		
	}
	
	long timeStamp = -1;

	public void searchResultChanged(SearchResultEvent e) {
		if(search == null) return;
		if(timeStamp == search.getTimeStamp()) return;
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				if(viewer == null || timeStamp == search.getTimeStamp()) return;
				viewer.refresh();
				timeStamp = search.getTimeStamp();
				part.updateLabel();
			}
		});
	}
	
	class MetaLabelProvider extends NavigatorLabelProvider {
		public String getText(Object element) {
			if(element instanceof XModelObject) {
				XModelObject o = (XModelObject)element;
				XModelObject f = o;
				while(f != null && f.getFileType() == XModelObject.NONE) f = f.getParent();
				String file = f == null ? "" : FileAnyImpl.toFileName(f);
				String entity = o.getModelEntity().getName();
				if("MetaEntity".equals(entity)) {
					return super.getText(element) + " - " + file;
				} else if("MetaAttribute".equals(entity)) {
					XModelObject p = o.getParent().getParent();
					return super.getText(element) + " - entity " + p.getPresentationString() + " - " + file;
				} else {
					return super.getText(element) + " - " + file;
				}
			}
			return super.getText(element);
		}
		
	}
	
}
