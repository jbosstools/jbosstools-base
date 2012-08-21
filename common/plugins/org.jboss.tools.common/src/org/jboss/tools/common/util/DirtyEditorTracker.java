/*******************************************************************************
  * Copyright (c) 2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributors:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.util;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IPageListener;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IPropertyListener;
import org.eclipse.ui.IWindowListener;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.CommonPlugin;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class DirtyEditorTracker implements IWindowListener, IPageListener, IPartListener, IPropertyListener {
	static DirtyEditorTracker INSTANCE;

	private Set<IFile> dirtyFiles = new HashSet<IFile>();
	private Map<IFile, Integer> openEditors = new HashMap<IFile, Integer>();

	private DirtyEditorTracker() {
		init();
	}

	public static DirtyEditorTracker getInstance() {
		if(INSTANCE == null) {
			INSTANCE = new DirtyEditorTracker();
		}
		return INSTANCE;
	}

	private void init() {
		if( PlatformUI.isWorkbenchRunning() ) {
			IWorkbench workbench = CommonPlugin.getDefault().getWorkbench();
			if(workbench != null) {
				IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();
				for (IWorkbenchWindow window: windows) {
					windowOpened(window);
				}
				CommonPlugin.getDefault().getWorkbench().addWindowListener(this);
			}
		}
	}

	public Set<IFile> getDirtyFiles() {
		Set<IFile> result = new HashSet<IFile>();
		synchronized(this) {
			result.addAll(dirtyFiles);
		}
		return result;
	}

	public synchronized boolean isDirty(IFile file) {
		return dirtyFiles.contains(file);
	}

	@Override
	public void windowActivated(IWorkbenchWindow window) {
	}

	@Override
	public void windowDeactivated(IWorkbenchWindow window) {
	}

	@Override
	public void windowClosed(IWorkbenchWindow window) {
		IWorkbenchPage[] pages = window.getPages();
		for (IWorkbenchPage page: pages) {
			pageClosed(page);
		}
		window.removePageListener(this);
	}

	@Override
	public void windowOpened(IWorkbenchWindow window) {
		if(window.getShell() != null) {
			IWorkbenchPage[] pages = window.getPages();
			for (IWorkbenchPage page: pages) {
				pageOpened(page);
			}
			window.addPageListener(this);
		}
	}

	@Override
	public void pageActivated(IWorkbenchPage page) {
	}

	@Override
	public void pageClosed(IWorkbenchPage page) {
		IEditorReference[] rs = page.getEditorReferences();
		for (IEditorReference r: rs) {
			IEditorPart part = r.getEditor(false);
			if(part != null) {
				editorClosed(part);
			}
		}
		page.removePartListener(this);
	}

	@Override
	public void pageOpened(IWorkbenchPage page) {
		IEditorReference[] rs = page.getEditorReferences();
		for (IEditorReference r: rs) {
			IEditorPart part = r.getEditor(false);
			if(part != null) {
				editorOpened(part);
			}							
		}
		page.addPartListener(this);
	}

	@Override
	public void partActivated(IWorkbenchPart part) {
	}

	@Override
	public void partBroughtToTop(IWorkbenchPart part) {
	}

	@Override
	public void partClosed(IWorkbenchPart part) {
		if (part instanceof IEditorPart) {
			editorClosed((IEditorPart)part);
		}
	}

	@Override
	public void partDeactivated(IWorkbenchPart part) {
	}

	@Override
	public void partOpened(IWorkbenchPart part) {
		if (part instanceof IEditorPart) {
			editorOpened((IEditorPart)part);
		}
	}

	private void editorOpened(IEditorPart part) {
		IFile file = getFile(part);
		if(file != null) {
			part.addPropertyListener(this);
			synchronized (this) {
				Integer i = openEditors.get(file);
				int k = i == null ? 1 : i.intValue() + 1;
				openEditors.put(file, k);
			}
			update(part);
		}
	}

	private void editorClosed(IEditorPart part) {
		part.removePropertyListener(this);
		IFile file = getFile(part);
		if(file != null) {
			synchronized (this) {
				Integer i = openEditors.get(file);
				if(i != null) {
					if(i < 2) {
						openEditors.remove(file);
						update(file, false);
					} else {
						openEditors.put(file, i.intValue() - 1);
					}
				}
			}
		}
	}

	@Override
	public void propertyChanged(Object source, int propId) {
		if(propId == IEditorPart.PROP_DIRTY && source instanceof IEditorPart) {
			update((IEditorPart)source);
		}
	}

	private IFile getFile(IEditorPart part) {
		IEditorInput input = part.getEditorInput();
		return (input instanceof IFileEditorInput) ? ((IFileEditorInput)input).getFile() : null;
	}

	private void update(IEditorPart part) {
		IFile f = getFile(part);
		if(f != null) {
			update(f, part.isDirty());
		}
	}

	private synchronized void update(IFile file, boolean isDirty) {
		if(isDirty) {
			if(!dirtyFiles.contains(file)) {
				dirtyFiles.add(file);
			}
		} else if(dirtyFiles.contains(file)) {
			dirtyFiles.remove(file);
		}
	}
}
