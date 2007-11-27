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
package org.jboss.tools.common.model.ui.views.navigator;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.*;
import org.eclipse.ui.views.navigator.*;

public class NRefactorActionGroup extends ResourceNavigatorActionGroup {
	private Clipboard clipboard;
	private ResourceNavigatorRenameAction rename;
	private ResourceNavigatorMoveAction move;
	private NCopyAction copy;
	private NCutAction cut;
	private NDeleteAction delete;
	private NPasteAction paste;
	
	private BaseSelectionListenerAction[] actions;
	
	private TextActionHandler handler;
	
	public NRefactorActionGroup(IResourceNavigator navigator) {
		super(navigator);
	}

	public void dispose() {
		if (clipboard != null) {
			clipboard.dispose();
			clipboard = null;
		}
		super.dispose();
	}

	public void fillContextMenu(IMenuManager menu) {
		IStructuredSelection selection = (IStructuredSelection) getContext().getSelection();
		BaseSelectionListenerAction[] g = new BaseSelectionListenerAction[]{copy, cut, paste};
		add(menu, g, selection);
		if (isAny(selection)) {
			g = new BaseSelectionListenerAction[]{delete, move, rename};
			add(menu, g, selection);
		}
	}
	void add(IMenuManager menu, BaseSelectionListenerAction[] g, IStructuredSelection selection) {
		for (int i = 0; i < g.length; i++) {
			g[i].selectionChanged(selection);
			menu.add(g[i]);
		}
	}	
	boolean isAny(IStructuredSelection selection) {
		return !selection.isEmpty()	&& ResourceSelectionUtil.allResourcesAreOfType(
				selection, IResource.PROJECT | IResource.FOLDER | IResource.FILE);
	}

	public void fillActionBars(IActionBars actionBars) {
		handler = new TextActionHandler(actionBars);
		handler.setDeleteAction(delete);
		handler.setCopyAction(copy);
		handler.setCutAction(cut);
		handler.setPasteAction(paste);
		rename.setTextActionHandler(handler);
		actionBars.setGlobalActionHandler(ActionFactory.MOVE.getId(), move);
		actionBars.setGlobalActionHandler(ActionFactory.RENAME.getId(), rename);
	}

	public void handleKeyPressed(KeyEvent event) {
		if (isDelete(event)) {
			if (delete.isEnabled()) {
				delete.run();
			}
			event.doit = false;
		} else if (event.keyCode == SWT.F2 && event.stateMask == 0) {
			if (rename.isEnabled()) {
				rename.run();
			}
			event.doit = false;
		}
	}
	boolean isDelete(KeyEvent event) {
		return event.character == SWT.DEL && event.stateMask == 0;
	}

	protected void makeActions() {
		Shell shell = navigator.getSite().getShell();
		clipboard = new Clipboard(shell.getDisplay());
		ISharedImages images = PlatformUI.getWorkbench().getSharedImages();
		makePaste(shell, images);		
		makeCopy(shell, images);		
		makeCut(shell, images);		
		makeDelete(shell, images);
		move = new ResourceNavigatorMoveAction(shell, navigator.getViewer());
		rename = new ResourceNavigatorRenameAction(shell, navigator.getViewer());
		actions = new BaseSelectionListenerAction[]{
			copy, cut, delete, paste, rename, move
		};
	}
	
	private void makePaste(Shell shell, ISharedImages images) {
		paste = new NPasteAction(shell, clipboard);
		dressAction(paste, shell, images, ISharedImages.IMG_TOOL_PASTE_DISABLED, ISharedImages.IMG_TOOL_PASTE);
	}
	private void makeCopy(Shell shell, ISharedImages images) {
		copy = new NCopyAction(shell, clipboard, paste);
		dressAction(copy, shell, images, ISharedImages.IMG_TOOL_COPY_DISABLED, ISharedImages.IMG_TOOL_COPY);
	}
	private void makeCut(Shell shell, ISharedImages images) {
		cut = new NCutAction(shell, paste);
		dressAction(cut, shell, images, ISharedImages.IMG_TOOL_CUT_DISABLED, ISharedImages.IMG_TOOL_CUT);
	}
	private void makeDelete(Shell shell, ISharedImages images) {
		delete = new NDeleteAction(shell);
		dressAction(delete, shell, images, ISharedImages.IMG_TOOL_DELETE_DISABLED, ISharedImages.IMG_TOOL_DELETE);
	}
	private void dressAction(BaseSelectionListenerAction action, Shell shell, ISharedImages images, String disabledId, String enabledId) {
		if(disabledId != null) {
			action.setDisabledImageDescriptor(images.getImageDescriptor(disabledId));
		}
		if(enabledId != null) {
			action.setImageDescriptor(images.getImageDescriptor(enabledId));
		}
	}

	public void updateActionBars() {
		if(actions == null) return;
		IStructuredSelection selection = (IStructuredSelection) getContext().getSelection();
		for (int i = 0; i < actions.length; i++) actions[i].selectionChanged(selection);
	}
	
}
