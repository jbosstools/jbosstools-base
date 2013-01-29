/******************************************************************************* 
 * Copyright (c) 2013 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.test.util;

import java.io.File;
import java.io.FileFilter;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;
import org.eclipse.ui.dialogs.IOverwriteQuery;
import org.eclipse.ui.wizards.datatransfer.ImportOperation;
import org.jboss.tools.tests.ImportProvider;

public class ImportProjectsApplication implements IApplication {
	
	ProjectFilter filter = new ProjectFilter();

	@Override
	public Object start(IApplicationContext context) throws Exception {
		String rootPath = getRootPath(context);
		if(rootPath != null && !rootPath.isEmpty()){
			System.out.println("Root Path - "+rootPath);
			ProjectInfo[] projectsInfo = getProjectsInfo(rootPath);
		
			importProjects(projectsInfo);
		}
		return EXIT_OK;
	}

	@Override
	public void stop() {
	}
	
	private String getRootPath(IApplicationContext context){
		String[] rootPath = (String[])context.getArguments().get(IApplicationContext.APPLICATION_ARGS);
		if(rootPath != null && rootPath.length > 0){
			return rootPath[0];
		}else{
			return null;
		}
	}
	
	private ProjectInfo[] getProjectsInfo(String rootPath){
		File root = new File(rootPath);
		ArrayList<ProjectInfo> infos = new ArrayList<ProjectInfo>();
		
		getProjectInfo(root, infos);
		
		return infos.toArray(new ProjectInfo[]{});
	}
	
	private void getProjectInfo(File root, ArrayList<ProjectInfo> infos){
		if(root.isDirectory()){
			File[] projectFiles = root.listFiles(filter);
			for(File file : projectFiles){
				System.out.println("PROJECT FOUND - "+root.getName());
				ProjectInfo info = new ProjectInfo(root.getName(), root.getPath());
				infos.add(info);
				return;
			}
			
			for(File file : root.listFiles()){
				if(file.isDirectory()){
					getProjectInfo(file, infos);
				}
			}
		}
		return;
	}
	
	private IProject[] importProjects(ProjectInfo[] projectsInfo) throws Exception {
		IProject[] projects = new IProject[projectsInfo.length]; 
		for (int i = 0; i < projectsInfo.length; i++) {
			System.out.println("Import - "+projectsInfo[i].projectName);
			projects[i] = importProjectIntoWorkspace(projectsInfo[i].projectPath, projectsInfo[i].projectName);
		}
		return projects;
	}
	
	static public IProject importProjectIntoWorkspace(String projectPath, String projectName) {
		IProject project = null;
		
			try {
				 boolean state = ResourcesUtils.setBuildAutomatically(false);
				 project = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
				 if (!project.exists())
					 project.create(null);
				 
				 project.open(null);

				 IOverwriteQuery overwrite = new IOverwriteQuery() {
					public String queryOverwrite(String pathString) {
						return ALL;
					}
				};

				ImportProvider importProvider = new ImportProvider();

				// create import operation
				ImportOperation importOp = new ImportOperation(project
						.getFullPath(), new File(projectPath), importProvider, overwrite);
				importOp.setCreateLinks(true);	
				
				// import files just to project folder ( without old structure )
				importOp.setCreateContainerStructure(false);

				// run import
				importOp.run(null);
				ResourcesUtils.setBuildAutomatically(state);
			} catch (InvocationTargetException ite) {
				ite.printStackTrace();
			} catch (InterruptedException ie) {
				ie.printStackTrace();
			} catch (CoreException e) {
				e.printStackTrace();
			}
			return project;
		}
	
	class ProjectInfo{
		String projectPath;
		String projectName;
		
		public ProjectInfo(String name, String path){
			this.projectName = name;
			this.projectPath = path;
		}
	}
	
	class ProjectFilter implements FileFilter{

		@Override
		public boolean accept(File file) {
			if(".project".equals(file.getName())){
				return true;
			}
			return false;
		}
		
	}
}
