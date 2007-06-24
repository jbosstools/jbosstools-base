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
package org.jboss.tools.common.model.ui.dialog;

import java.util.*;
import java.util.List;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.jboss.tools.common.model.ui.wizards.one.ServiceDialogImpl;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.dialogs.ElementTreeSelectionDialog;
import org.eclipse.ui.dialogs.ISelectionStatusValidator;
import org.eclipse.ui.model.*;
import org.eclipse.ui.views.navigator.ResourceSorter;

import org.jboss.tools.common.meta.action.SpecialWizard;

public class SelectEclipseFileWizard implements SpecialWizard {
	Properties p;

	public void setObject(Object object) {
		p = (Properties)object;
	}

	public int execute() {
		String[] extensions = getExtensions(p.getProperty("extension"));
		
		FSDialog d = new FSDialog(
			ServiceDialogImpl.getShell(), 
			(IResource)p.get("root"), 
			new ArrayList(), 
			"Select File", 
			p.getProperty("message"),
			extensions, 
			getFilterName(extensions), 
			p.getProperty("selection"));

		if(d.open() != FSDialog.OK) return 1;
		Object result = d.getFirstResult();
		if(result != null) p.put("result", result);
		return 0;
	}
	
	String[] getExtensions(String extension) {
		if(extension == null || extension.length() == 0 || extension.equals("*")) return null;
		StringTokenizer st = new StringTokenizer(extension, ",;");
		String[] r = new String[st.countTokens()];
		for (int i = 0; i < r.length; i++) {
			r[i] = st.nextToken().trim();
		}
		return r;
	}
	String getFilterName(String[] extensions) {
		if(extensions == null) return null;
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < extensions.length; i++) {
			if(sb.length() > 0) sb.append(", ");
			sb.append("*.").append(extensions[i]);
		}
		return "Show only " + sb.toString() + " files";
	}

}

class FSDialog extends ElementTreeSelectionDialog {
	private String m;
	private String seed;
	FileFilter z;
    boolean everything = true;
	
	public FSDialog(Shell parent, IResource root, List files, String title, String message, String[] extensions, String mess, String seed) {
		super(parent, new WorkbenchLabelProvider(), new WorkbenchContentProvider());
		this.seed = seed;
		setTitle(title);
		setMessage(message);
		z = new FileFilter(files, extensions);
		m = mess;
		everything = true;
		setInput(root != null ? root : ResourcesPlugin.getWorkspace().getRoot());	
		setSorter(new ResourceSorter(ResourceSorter.NAME));
		setValidator(createValidator());
	}
	private ISelectionStatusValidator createValidator() {
		return new ISelectionStatusValidator() {
			public IStatus validate(Object[] os) {
				if (os.length == 0) {
					return new Status(IStatus.ERROR, "org.jboss.tools.common.model.ui", 0, "", null); //$NON-NLS-1$
				}
				for (int i = 0; i < os.length; i++) {
					if (os[i] instanceof IFile) continue;
					return new Status(IStatus.ERROR, "org.jboss.tools.common.model.ui", 0, "", null); //$NON-NLS-1$
				}
				return new Status(IStatus.OK, "org.jboss.tools.common.model.ui", 0, "", null); //$NON-NLS-1$
			}			
		};
	}
	
	protected Control createDialogArea(Composite parent) {
		Composite result = (Composite)super.createDialogArea(parent);
		if(m != null) {
			Button b = createButton(parent, result);
			setSeed();
			z.considerExtension(everything);
			getTreeViewer().addFilter(z);
			if (!everything) b.setSelection(true);
		
			b.addSelectionListener(createListener(b));
		}
		applyDialogFont(result);		
		return result;
	}
	private Button createButton(Composite parent, Composite p2) {
		Button b = new Button(p2, SWT.CHECK);
		b.setText(m);
		b.setFont(parent.getFont());
		return b;
	}
	private void setSeed() {
		if (seed == null) return;
		IPath path = Path.fromPortableString(seed);
		IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember(path);
		setInitialSelection(resource);
	}
	private SelectionListener createListener(final Button b) {
		return new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				everything = !b.getSelection();
				z.considerExtension(!everything);
				getTreeViewer().refresh();
			}
		};
	}
	
}

class FileFilter extends ViewerFilter {
	private List fFilter;
	private Set<IResource> fFiles;
	private Set<String> fExtensions = new HashSet<String>();
    private boolean fConsiderExtension = true;

	public FileFilter(List objects, String[] extensions) {
		fFilter = objects;
		if(extensions == null || extensions.length == 0) {
			fConsiderExtension = false;
			fExtensions = null;
		} else {
			for (int i = 0; i < extensions.length; i++) fExtensions.add(extensions[i]);
		}
	}

	public boolean select(Viewer viewer, Object parentElement, Object element) {
		return fFiles.contains(element) && !fFilter.contains(element);
	}
	
	private void init() {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			public void run() {
				fFiles = new HashSet<IResource>();
				collect(ResourcesPlugin.getWorkspace().getRoot(), fFiles);
			}
		});
	}

	private boolean collect(IContainer container, Set<IResource> set) {
		boolean added = false;
		try {
			IResource[] resources = container.members();
			for (int i = 0; i < resources.length; i++) {
				IResource resource = resources[i];
				if (resource instanceof IFile) {
					IFile file = (IFile) resource;
					if (checkFile(file)) {
						set.add(file);
						added = true;
					}
				} else if (resource instanceof IContainer) {
					if (collect((IContainer) resource, set)) {
						set.add(resource);
						added = true;
					}
				}
			}
		} catch (CoreException e) {
		}
		return added;
	}
	private boolean checkFile(IFile file) {
		String ext = file.getFileExtension();
		if(!fConsiderExtension || fExtensions == null || ext == null) return false;
		return (fExtensions.contains(ext));
	}
	
	public void considerExtension(boolean considerExtension) {
	    fConsiderExtension= considerExtension;
	    init();
	}

}
