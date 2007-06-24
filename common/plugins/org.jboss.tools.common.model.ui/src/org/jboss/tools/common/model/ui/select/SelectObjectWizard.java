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
package org.jboss.tools.common.model.ui.select;

import java.util.*;

import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.XFileObject;
import org.jboss.tools.common.model.util.FindObjectHelper;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.EditorPartWrapper;

import org.jboss.tools.common.editor.ObjectMultiPageEditor;
import org.jboss.tools.common.model.ui.dnd.*;
import org.eclipse.jface.viewers.*;

public class SelectObjectWizard implements SpecialWizard {
	private XModelObject object = null;
	private int where = FindObjectHelper.EVERY_WHERE;
	private String preferredPage = null;

	public void setObject(Object object) {
		if(object instanceof Integer) {
			where = ((Integer)object).intValue();
		} else if(object instanceof XModelObject) {
			this.object = (XModelObject)object;
		} else if(object instanceof String) {
			preferredPage = object.toString();
		}
	}

	public int execute() {
		if(object == null) return 0;
		XModelObject of = selectInNavigator();
		if(of == null) return 1;
		final IEditorPart p = openEditor(getObjectToOpen((XModelObject)object));
		if(p != null && p.getSite().getSelectionProvider() != null && (object instanceof XModelObject)) {
			Display.getDefault().asyncExec(new Runnable() {
				public void run() {
					if(preferredPage != null && p instanceof EditorPartWrapper) {
						EditorPartWrapper w = (EditorPartWrapper)p;
						if(w.getEditor() instanceof ObjectMultiPageEditor) {
							ObjectMultiPageEditor oe = (ObjectMultiPageEditor)w.getEditor();
							oe.selectPageByName(preferredPage);
						}
					}
					try {
						p.getSite().getSelectionProvider().setSelection(getSelection((XModelObject)object));
					} catch (Exception e) {
						// we cannot avoid exception if third party editor is used
					}
					IWorkbenchPage page = ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();
					IViewPart vs = page.findView("org.eclipse.ui.views.ContentOutline");
					ISelectionProvider sp = vs == null ? null : vs.getSite().getSelectionProvider();
					if(sp != null) {
						try {
							sp.setSelection(getSelection(object));
						} catch (Exception e) {
							// we cannot avoid exception if third party outline is used
						}
					}
				}
			});
		}
		return 0;
	}
	
	private StructuredSelection getSelection(XModelObject o) {
		return new StructuredSelection(new Object[]{o});
	}
	
	String[] views = new String[]{
		"org.jboss.tools.jst.web.ui.navigator.WebProjectsView"
//		"org.jboss.tools.jsf.ui.navigator.JsfProjectsView",
//		"org.jboss.tools.struts.ui.navigator.StrutsProjectsView",
//		"org.jboss.tools.common.model.ui.navigator.NavigatorViewPart"
	};

	private XModelObject selectInNavigator() {
		if(!(object instanceof XModelObject)) return null;
		XModelObject o = (XModelObject)object, of = getObjectToOpen(o);
		boolean doSelect = where != FindObjectHelper.IN_EDITOR_ONLY;
		if(!doSelect) return of;
		IWorkbenchPage page = ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();
		for (int i = 0; i < views.length; i++) {
			IViewPart navigatorpart = page.findView(views[i]);
			if(navigatorpart == null || navigatorpart.getSite() == null) continue;
			ISelectionProvider sp = navigatorpart.getSite().getSelectionProvider();
			if(sp == null) continue;
			sp.setSelection(getSelection(o));
			if(getSelection(navigatorpart) == o
					// Actually, this is next to never show WebProjectsView.
					&& where == FindObjectHelper.IN_NAVIGATOR_ONLY) {
				try { page.showView(views[i]); } catch (Exception e) {}
			}
		}
		return of;
	}

	private XModelObject getObjectToOpen(XModelObject o) {
		if(where == FindObjectHelper.IN_NAVIGATOR_ONLY) return null;
		// First try to find parent file.
		XModelObject ofile = o;
		while(ofile != null && ofile.getFileType() == XModelObject.NONE) ofile = ofile.getParent();
		if(ofile != null && DnDUtil.getEnabledAction(ofile, null, "Open") != null) return ofile;

		while(o != null) {
			if(DnDUtil.getEnabledAction(o, null, "Open") != null) return o;
			if(o.getFileType() != XFileObject.NONE) return null;
			o = o.getParent();
		}
		return null;
	}

	private IEditorPart openEditor(XModelObject of) {
		Properties p = new Properties();
		if(where == FindObjectHelper.IN_NAVIGATOR_AND_IN_EDITOR_IF_OPEN) {
			p.setProperty("onlySelectIfOpen", "true");
		}
		if(of != null && XActionInvoker.getAction("Open", of) != null) {
			XActionInvoker.invoke("Open", of, p);
		}
		return (IEditorPart)p.get("editor");
	}
	
	private XModelObject getSelection(IViewPart part) {
		ISelection s = part.getSite().getSelectionProvider().getSelection();
		if(!(s instanceof StructuredSelection)) return null;
		Object o = ((StructuredSelection)s).getFirstElement();
		return (o instanceof XModelObject) ? (XModelObject)o : null;		
	}

}
