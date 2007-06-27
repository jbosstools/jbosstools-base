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
package org.jboss.tools.common.editor;

import java.util.StringTokenizer;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.IPath;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.*;
import org.eclipse.ui.part.*;
import org.jboss.tools.common.core.resources.XModelObjectEditorInput;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.*;

public class XMLEditorLauncher implements IEditorLauncher {

	public void open(IFile file) {
		FileEditorInput f = new FileEditorInput(file);
		IEditorInput input = XModelObjectEditorInput.checkInput(f);
		String entity = computeEntity(input);
		EditorPartWrapperExtension extension = EditorPartWrapperExtension.getInstance();
		EditorPartFactory factory = extension.getFactory(entity);
		String id = factory.getEditorId();
		if(id == null) id = "org.jboss.tools.common.text.xml.internal.ui.xmleditor.XmlEditor";
		new Thread(new LaunchThread(id, input)).start();		
	}

	static String DEFAULT_ENTITY = "xml";
	
	private String computeEntity(IEditorInput input) {
		if(!(input instanceof IModelObjectEditorInput)) return DEFAULT_ENTITY; 
		IModelObjectEditorInput i = (IModelObjectEditorInput)input;
		XModelObject o = i.getXModelObject();
		if(o == null) return DEFAULT_ENTITY;
		EditorPartWrapperExtension extension = EditorPartWrapperExtension.getInstance();
		EditorPartFactory f = extension.getFactory(o.getModelEntity().getName());
		if(f == null) return DEFAULT_ENTITY;
		return o.getModelEntity().getName();
	}
	
	class LaunchThread implements Runnable {
		String id;
		IEditorInput input;
		LaunchThread(String id, IEditorInput input) {
			this.id = id;
			this.input = input;
		}
		public void run() {
			Display.getDefault().syncExec( 
				new Runnable() {
					public void run() {
						IWorkbenchPage workbenchPage = ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();
						int time = 100;
						while(workbenchPage.findEditor(input) != null) {
							if(time > 1000) return;
							try { Thread.sleep(time); } catch (Exception e) {}
							time *= 2;
						}
						try {
							workbenchPage.openEditor(input, id);			
						} catch (Exception e) {
							ModelUIPlugin.getPluginLog().logError((Exception)e);
						}
					}
				}
			);
		}
	}

	public static IFile convert(IPath path) {
		String location = path.toFile().getAbsolutePath();
		IProject[] projects = ModelUIPlugin.getWorkspace().getRoot().getProjects();
		for (int i = 0; projects != null && i < projects.length; i++) {
			if (!projects[i].isOpen()) continue;
			IFile f = find(projects[i], location);
			if(f != null) return f;
			IResource[] rs = null;
			try {
				rs = projects[i].members(true);
			} catch (Exception e) {
				//ignore
			}
			if(rs != null) for (int j = 0; j < rs.length; j++) {
				if(!rs[j].isLinked()) continue;
				f = find(rs[j], location);
				if(f != null) return f;
			}
		}
		return ModelUIPlugin.getWorkspace().getRoot().getFile(path);
	}
	
	public void open(IPath path) {
		open(convert(path));
	}
	
	private static IFile find(IResource resource, String location) {
		String l = resource.getLocation().toFile().getAbsolutePath();
		if (!location.startsWith(l)) return null;
		String relative = location.substring(l.length()).replace('\\', '/');
		StringTokenizer st = new StringTokenizer(relative, "/");
		
		while(st.hasMoreTokens()) {
			String token = st.nextToken();
			if (resource instanceof IContainer) {
				IContainer container = (IContainer)resource;
				resource = container.findMember(token);
				if (resource == null) break;
				else if(!st.hasMoreTokens() && resource instanceof IFile) return (IFile)resource;
			} else break;
		}
		
		return null;
	}

}
