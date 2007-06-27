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
package org.jboss.tools.common.model.project;

import java.util.*;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.ClassLoaderUtil;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

public abstract class ModelNature extends PlatformObject implements IProjectNature, IModelNature {
	public static final String NATURE_ID = ModelPlugin.PLUGIN_ID + ".modelnature";
	static Map<IProject,XModel> models = new HashMap<IProject,XModel>();
	static IResourceChangeListener listener = null;
	protected String workspaceHome = null;
	protected IProject project = null;
	protected XModel model = null;
	
	public ModelNature() {}

	public void configure() throws CoreException {
	}

	public void deconfigure() throws CoreException {
		ModelPlugin p = ModelPlugin.getDefault();
		if(model != null) {
			p.getSaveParticipant().removeModel(model);
			model = null;
		}		
	}
	
	public IProject getProject() {
		return project;
	}

	public void setProject(IProject project) {
		if(this.project == project) return;		
		this.project = project;
		try {
			createProject();
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError("ModelNature:createProject()", e);
		}
	}
	
	public Object getAdapter(Class adapter) {
		return super.getAdapter(adapter);
	}
	
	public XModel getModel() {
		return model;
	}
	
	private void updateListener() {
		if(listener != null) return;
		listener = new Listener();
		ModelPlugin.getWorkspace().addResourceChangeListener(listener);
	}
	
	static class Listener implements IResourceChangeListener {
		public void resourceChanged(IResourceChangeEvent event) {
			if(event.getType() == IResourceChangeEvent.PRE_DELETE) {
				IProject p = event.getResource().getProject();
				if(p != null && models.containsKey(p)) {
					models.remove(p);
				}				
			}
		}
	}
	
	private void createProject() {
//@S_CHECK@
		ClassLoaderUtil.init();
        model = (XModel)models.get(project);
        if(model != null) {
			String home = getWorkspaceHome();
			String h = XModelConstants.getWorkspace(model);
			if(home == null || !home.equals(h)) {
				ModelPlugin.getPluginLog().logInfo("WARNING:" + " workspace home changed from " + h + " to " + home);
				model.getProperties().setProperty(XModelConstants.WORKSPACE, home);
				model.getProperties().setProperty("nature", getID());
				model.load();				
			}
        	return;
        } 
		Properties p = new Properties();
		p.putAll(System.getProperties());
		p.setProperty(XModelConstants.WORKSPACE, getWorkspaceHome());
		p.setProperty(ECLIPSE_PROJECT, project.getLocation().toString());
		p.setProperty(ECLIPSE_PROJECT_OLD, project.getLocation().toString());
		p.put("project", project);
		p.setProperty("nature", getID());
		model = XModelFactory.getModel(p);
		if(model.getService() == null) {
			model.setService(createServiceDialog());		
		}
		models.put(project, model);
		ModelPlugin.getDefault().getSaveParticipant().addModel(model);
		try {
			updateProjectVersion();
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError(e);
		}
		updateListener();
	}
	
	protected void addToBuildSpec(String builderID) throws CoreException {
		IProjectDescription description = getProject().getDescription();
		ICommand command = null;
		ICommand commands[] = description.getBuildSpec();
		for (int i = 0; i < commands.length && command == null; ++i) {
			if (commands[i].getBuilderName().equals(builderID)) 
				command = commands[i];
		}
		if (command == null) {
			command = description.newCommand();
			command.setBuilderName(builderID);
			ICommand[] oldCommands = description.getBuildSpec();
			ICommand[] newCommands = new ICommand[oldCommands.length + 1];
			System.arraycopy(oldCommands, 0, newCommands, 0, oldCommands.length);
			newCommands[oldCommands.length] = command;
			description.setBuildSpec(newCommands);
			getProject().setDescription(description, null);
		}
	}
	
	static String EXTERNAL_TOOL_BUILDER = "org.eclipse.ui.externaltools.ExternalToolBuilder";
	static final String LAUNCH_CONFIG_HANDLE = "LaunchConfigHandle";
	
	protected void removeFromBuildSpec(String builderID) throws CoreException {
		IProjectDescription description = getProject().getDescription();
		ICommand[] commands = description.getBuildSpec();
		for (int i = 0; i < commands.length; ++i) {
			String builderName = commands[i].getBuilderName();
			if (!builderName.equals(builderID)) {
				if(!builderName.equals(EXTERNAL_TOOL_BUILDER)) continue;
				Object handle = commands[i].getArguments().get(LAUNCH_CONFIG_HANDLE);
				if(handle == null || handle.toString().indexOf(builderID) < 0) continue;
			}
			ICommand[] newCommands = new ICommand[commands.length - 1];
			System.arraycopy(commands, 0, newCommands, 0, i);
			System.arraycopy(commands, i + 1, newCommands, i, commands.length - i - 1);
			description.setBuildSpec(newCommands);
			getProject().setDescription(description, null);
			return;
		}
	}
	
	protected void updateProjectVersion()	{
	}
	
	private String getWorkspaceHome() {
		return new ProjectHome().getLocation(project);
	}
	
	
	private ServiceDialog createServiceDialog() {
		try {
			return (ServiceDialog)ModelFeatureFactory.getInstance().createFeatureInstance("org.jboss.tools.common.model.ui.wizards.one.ServiceDialogImpl");
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError("Cannot create service dialog.");
		}
		return null;
	}

}
