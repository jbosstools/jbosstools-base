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
package org.jboss.tools.common.model.ui.views.palette;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Properties;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.MultiPageEditorPart;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.texteditor.ITextEditor;
import org.osgi.framework.Bundle;

import org.jboss.tools.common.meta.action.SpecialWizard;
import org.jboss.tools.common.meta.action.SpecialWizardFactory;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.dnd.ModelTransfer;
import org.jboss.tools.common.model.ui.editor.EditorPartWrapper;

public class PaletteViewPart extends ViewPart implements IPartListener {
	
	public static final String VIEW_ID = "org.jboss.tools.common.model.ui.views.palette.PaletteView"; //$NON-NLS-1$
	public static final String PALETTE_GEF_ID = "org.jboss.tools.vpe.ui.palette"; //$NON-NLS-1$

	private IPaletteAdapter paletteAdapter = null; 
	private Composite root = null;
	private Control palette = null;
	private IWorkbenchPart lastPart = null;
	private boolean lastPaletteEnabled = false;

	public void createPartControl(Composite parent) {
		createPartControlImpl(parent);
	}

	protected void setContentDescription(String description) {
		super.setContentDescription(description);
	}

	private void createPartControlImpl(Composite parent) {
		paletteAdapter = createPaletteAdapter();
		paletteAdapter.setPaletteContents(null);
		paletteAdapter.setPaletteViewPart(this);
		root = new Composite(parent, SWT.NONE);
		root.setLayout(new FillLayout());
		palette = paletteAdapter.createControl(root);
	}
	
	private IPaletteAdapter createPaletteAdapter() {
		try {
			Bundle b = Platform.getBundle(PALETTE_GEF_ID);
			Class cls = b == null ? null : b.loadClass("org.jboss.tools.vpe.ui.palette.PaletteAdapter"); //$NON-NLS-1$
			if(cls != null) return (IPaletteAdapter)cls.newInstance();
		} catch (ClassNotFoundException t) {
			//ignore
		} catch (InstantiationException e) {
			//ignore
		} catch (IllegalAccessException e) {
			//ignore
		}
		return new PaletteAdapter();
	}

	public void dispose() {
		super.dispose();
		if (palette != null) {
			if (!palette.isDisposed())
				palette.dispose();
			palette = null;
		}
		if (root != null) {
			if (!root.isDisposed())
				root.dispose();
			root = null;
		}
		getSite().getPage().removePartListener(this);
		if (paletteAdapter != null)
			paletteAdapter.dispose();
		paletteAdapter = null;
	}

	public void setFocus() {
		if(palette != null && !palette.isDisposed()) palette.setFocus();
	}

	public void partActivated(IWorkbenchPart part) {
	    IEditorPart part1  = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		if (part1 instanceof IEditorPart) {
			paletteAdapter.setPaletteContents(new PaletteContents((IEditorPart)part1));
			lastPaletteEnabled = true;
			paletteAdapter.setEnabled(lastPaletteEnabled);
			lastPart = part1;
		} else {
			paletteAdapter.setEnabled(false);
		}
			
	}

	public void partBroughtToTop(IWorkbenchPart part) {
	}

	public void partClosed(IWorkbenchPart part) {
		if (lastPart != null && getSite().getPage().getActiveEditor() != lastPart) {
			lastPart = null;
			paletteAdapter.setPaletteContents(new PaletteContents(null));
			paletteAdapter.setEnabled(lastPaletteEnabled = false);
		}
	}

	public void partDeactivated(IWorkbenchPart part) {}

	public void partOpened(IWorkbenchPart part) {}

	public void insertIntoEditor(XModelObject macro) {
		IWorkbenchPage page = getSite().getPage();
		IEditorPart part = page.getActiveEditor();
		if (part == null) return;
		ITextEditor editor = getActiveTextEditor(part);
		
		/// It is not a text editor
		if(editor == null) return;
		/// Do we need a warning here?
		if(!editor.isEditable()) return;
	
		if(dropIntoEditor(editor, macro)) return;
		if(dropIntoEditor2(editor, macro)) return;

		//TODO implement a service
/**
		String tagname = macro.getAttributeValue("name"); //$NON-NLS-1$
		String startText = "" + macro.getAttributeValue("start text"); //$NON-NLS-1$ //$NON-NLS-2$
		String endText = "" + macro.getAttributeValue("end text"); //$NON-NLS-1$ //$NON-NLS-2$
		String newline = "" + macro.getAttributeValue("new line"); //$NON-NLS-1$ //$NON-NLS-2$
		String reformat = "" + macro.getAttributeValue("automatically reformat tag body"); //$NON-NLS-1$ //$NON-NLS-2$
		XModelObject parent = macro.getParent();
		String uri = (parent == null) ? "" : parent.getAttributeValue(URIConstants.LIBRARY_URI); //$NON-NLS-1$
		String libraryVersion = (parent == null) ? "" : parent.getAttributeValue(URIConstants.LIBRARY_VERSION); //$NON-NLS-1$
		String defaultPrefix = (parent == null) ? "" : parent.getAttributeValue(URIConstants.DEFAULT_PREFIX); //$NON-NLS-1$
		String addTaglib = (parent == null) ? "" : parent.getAttributeValue("add taglib"); //$NON-NLS-1$ //$NON-NLS-2$
		if (editor != null) {
			Properties properties = new Properties();
			properties.setProperty(PaletteInsertHelper.PROPOPERTY_TAG_NAME, tagname);
			properties.setProperty(PaletteInsertHelper.PROPOPERTY_TAGLIBRARY_URI, uri);
			properties.setProperty(PaletteInsertHelper.PROPOPERTY_TAGLIBRARY_VERSION, libraryVersion);
			properties.setProperty(PaletteInsertHelper.PROPOPERTY_DEFAULT_PREFIX, defaultPrefix);

			if(startText != null) properties.setProperty(PaletteInsertHelper.PROPOPERTY_START_TEXT, startText);
			if(endText != null) properties.setProperty(PaletteInsertHelper.PROPOPERTY_END_TEXT, endText);
			properties.setProperty(PaletteInsertHelper.PROPOPERTY_REFORMAT_BODY, reformat);
			properties.setProperty(PaletteInsertHelper.PROPOPERTY_NEW_LINE, newline);
			properties.setProperty(PaletteInsertHelper.PROPOPERTY_ADD_TAGLIB, addTaglib);
			
			//TODO WARNING we cannot have here correct insert helper instance!
			PaletteInsertHelper.getInstance().insertIntoEditor(
					editor,
					properties
			);
			page.activate(part);
		}
*/
	}

	private boolean dropIntoEditor(ITextEditor editor, XModelObject macro) {
		try {
			Method m = editor.getClass().getMethod("runDropCommand", new Class[]{String.class, String.class}); //$NON-NLS-1$
			if(m == null) return false;
			m.setAccessible(true);
			Properties p = new Properties();
			p.setProperty("isDrag", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			XActionInvoker.invoke("CopyActions.Copy", macro, p); //$NON-NLS-1$
			m.invoke(editor, new Object[]{ModelTransfer.MODEL, null});
			return true;
		} catch (NoSuchMethodException ne) {
			return false;
		} catch (IllegalAccessException e) {
			ModelUIPlugin.getPluginLog().logError(e);
			return false;
		} catch (IllegalArgumentException e) {
			ModelUIPlugin.getPluginLog().logError(e);
			return false;
		} catch (InvocationTargetException e) {
			ModelUIPlugin.getPluginLog().logError(e);
			return false;
		}
	}
	private boolean dropIntoEditor2(ITextEditor editor, XModelObject macro) {
		SpecialWizard w = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.jst.jsp.outline.DefaultDropCommandRunner"); //$NON-NLS-1$
		if(w == null) return false;
		Properties p = new Properties();
		p.setProperty("isDrag", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		XActionInvoker.invoke("CopyActions.Copy", macro, p); //$NON-NLS-1$
		p.setProperty("flavor", ModelTransfer.MODEL); //$NON-NLS-1$
		p.put("textEditor", editor); //$NON-NLS-1$
		w.setObject(p);
		int r = w.execute();
		return r == 0;
	}

	public boolean idEnabled() {
		return lastPart != null; //lastTextEditor != null;
	}

	// TODO: NLS support
	private static final String ERROR_GETTING_ACTIVE_EDITOR = "Error while getting active text editor";
	
	// FIXME: Rewrite without reflection
	private ITextEditor getActiveTextEditor(IWorkbenchPart part) {
		ITextEditor editor = null;
		if (part instanceof EditorPartWrapper) {
			part = ((EditorPartWrapper)part).getEditor();
		}
		if (part instanceof ITextEditor) {
			editor = (ITextEditor)part;
		} else if (part instanceof MultiPageEditorPart) {
			try {
				Method m = MultiPageEditorPart.class.getDeclaredMethod("getActiveEditor", new Class[0]); //$NON-NLS-1$
				m.setAccessible(true);
				Object o = m.invoke(part, new Object[0]);
				if (o instanceof ITextEditor) {
					editor = (ITextEditor)o;
				}
			} catch (NoSuchMethodException t) {
				ModelUIPlugin.getPluginLog().logError(ERROR_GETTING_ACTIVE_EDITOR, t);
			} catch (IllegalArgumentException t) {
				ModelUIPlugin.getPluginLog().logError(ERROR_GETTING_ACTIVE_EDITOR, t);
			} catch (IllegalAccessException t) {
				ModelUIPlugin.getPluginLog().logError(ERROR_GETTING_ACTIVE_EDITOR, t);
			} catch (InvocationTargetException t) {
				ModelUIPlugin.getPluginLog().logError(ERROR_GETTING_ACTIVE_EDITOR, t);
			}
		}
		return editor;
	}

}
