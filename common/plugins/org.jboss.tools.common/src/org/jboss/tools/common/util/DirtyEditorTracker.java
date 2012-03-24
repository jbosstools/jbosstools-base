package org.jboss.tools.common.util;

import java.util.HashSet;
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
import org.jboss.tools.common.CommonPlugin;

public class DirtyEditorTracker implements IWindowListener, IPageListener, IPartListener, IPropertyListener {
	static DirtyEditorTracker INSTANCE;

	private Set<IFile> dirtyFiles = new HashSet<IFile>();

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
		IWorkbench workbench = CommonPlugin.getDefault().getWorkbench();
		if(workbench != null) {
			IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();
			for (IWorkbenchWindow window: windows) {
				if(window.getShell() != null) {
					IWorkbenchPage[] pages = window.getPages();
					for (IWorkbenchPage page: pages) {
						IEditorReference[] rs = page.getEditorReferences();
						for (IEditorReference r: rs) {
							IEditorPart part = r.getEditor(false);
							if(part != null) {
								update(part);
								part.addPropertyListener(this);
							}							
						}
						page.addPartListener(this);
					}
					window.addPageListener(this);
				}
			}
			CommonPlugin.getDefault().getWorkbench().addWindowListener(this);
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

	@Override
	public void pageActivated(IWorkbenchPage page) {
	}

	@Override
	public void pageClosed(IWorkbenchPage page) {
		page.removePartListener(this);
	}

	@Override
	public void pageOpened(IWorkbenchPage page) {
		page.addPartListener(this);
	}

	@Override
	public void windowActivated(IWorkbenchWindow window) {
	}

	@Override
	public void windowDeactivated(IWorkbenchWindow window) {
	}

	@Override
	public void windowClosed(IWorkbenchWindow window) {
		window.removePageListener(this);
	}

	@Override
	public void windowOpened(IWorkbenchWindow window) {
		window.addPageListener(this);
	}

	public void editorOpened(IEditorPart part) {
		IEditorInput input = part.getEditorInput();
		if(input instanceof IFileEditorInput) {
			part.addPropertyListener(this);
		}
	}

	public void editorClosed(IEditorPart part) {
		part.removePropertyListener(this);
	}

	@Override
	public void propertyChanged(Object source, int propId) {
		if(propId == IEditorPart.PROP_DIRTY && source instanceof IEditorPart) {
			IEditorPart part = (IEditorPart)source;
			update(part);
		}
		
	}

	private void update(IEditorPart part) {
		IEditorInput input = part.getEditorInput();
		if(input instanceof IFileEditorInput) {
			IFile f = ((IFileEditorInput)input).getFile();
			update(f, part.isDirty());
		}
	}

	private synchronized void update(IFile file, boolean isDirty) {
		if(isDirty) {
			if(!dirtyFiles.contains(file)) {
				dirtyFiles.add(file);
			}
		} else {
			if(dirtyFiles.contains(file)) {
				dirtyFiles.remove(file);
			}
		}
	}
}
