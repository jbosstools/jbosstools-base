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
	public static final String NATURE_ID = ModelPlugin.PLUGIN_ID + ".modelnature"; //$NON-NLS-1$
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
		createProject();
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
		ModelPlugin.getWorkspace().addResourceChangeListener(listener, IResourceChangeEvent.PRE_DELETE | IResourceChangeEvent.PRE_CLOSE);
	}
	
	static class Listener implements IResourceChangeListener {
		public void resourceChanged(IResourceChangeEvent event) {
			if(event.getType() == IResourceChangeEvent.PRE_DELETE
	//TODO It will be good to clean cache on close project, 
	//but it will need extensive testing, so let's do it later
	//when having more time before another release. 
//				|| event.getType() == IResourceChangeEvent.PRE_CLOSE 
			) {
				IProject p = event.getResource().getProject();
				if(p != null && models.containsKey(p)) {
					models.remove(p);
				}				
			}
		}
	}
	
	private void createProject() {
		ClassLoaderUtil.init();

		Properties p = new Properties();
		p.putAll(System.getProperties());
		
		String home = getWorkspaceHome();
		if(home != null && home.length() > 0) {
			p.setProperty(XModelConstants.WORKSPACE, home);
			p.setProperty(XModelConstants.WORKSPACE_OLD, home);
		} else {
			IAutoLoad auto = createAutoLoad();
			boolean result = auto != null && ProjectHome.getLocation(project, p);
			if(result) {
				p.put(XModelConstants.AUTOLOAD, auto);
			} else {
				p.setProperty(XModelConstants.WORKSPACE, ""); //$NON-NLS-1$
				p.setProperty(XModelConstants.WORKSPACE_OLD, ""); //$NON-NLS-1$
			}
		}
		p.setProperty(ECLIPSE_PROJECT, project.getLocation().toString());
		p.setProperty(ECLIPSE_PROJECT_OLD, project.getLocation().toString());
		p.put(XModelObjectConstants.PROJECT, project);
		p.setProperty("nature", getID()); //$NON-NLS-1$

		model = (XModel)models.get(project);
        if(model != null) {
			home = p.getProperty(XModelConstants.WORKSPACE);
			String h = XModelConstants.getWorkspace(model);
			if(home == null || !home.equals(h)) {
				ModelPlugin.getPluginLog().logInfo("WARNING:" + " workspace home changed from " + h + " to " + home); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				model.getProperties().setProperty(XModelConstants.WORKSPACE, home);
				model.getProperties().setProperty(XModelConstants.WORKSPACE_OLD, home);
				model.getProperties().setProperty("nature", getID()); //$NON-NLS-1$
				model.load();				
			}
        	return;
        }
		model = XModelFactory.getModel(p);
		if(model.getService() == null) {
			model.setService(createServiceDialog());		
		}
		models.put(project, model);
		ModelPlugin.getDefault().getSaveParticipant().addModel(model);
		updateProjectVersion();
		updateListener();
	}
	
	protected IAutoLoad createAutoLoad() {
		return null;
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
	
	static String EXTERNAL_TOOL_BUILDER = "org.eclipse.ui.externaltools.ExternalToolBuilder"; //$NON-NLS-1$
	static final String LAUNCH_CONFIG_HANDLE = "LaunchConfigHandle"; //$NON-NLS-1$
	
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
			return (ServiceDialog)ModelFeatureFactory.getInstance().createFeatureInstance("org.jboss.tools.common.model.ui.wizards.one.ServiceDialogImpl"); //$NON-NLS-1$
		} catch (ClassCastException e) {
			ModelPlugin.getPluginLog().logError("Cannot create service dialog."); //$NON-NLS-1$
		}
		return null;
	}

	/**
	 * These hack methods are used to prevent loading model nature
	 * when its project data is not stored in xmodel settings and should 
	 * be loaded from WTP, but project is not synchronized yet so that 
	 * loading will fail and xmodel will be corrupted.
	 * @param project
	 * @return
	 */
	public static boolean checkModelNature(IProject project) {
		if(project == null || !project.isOpen()) return false;
		String nature = null;
		ModelNatureExtension[] es = ModelNatureExtension.getInstances();
		try {
			for (ModelNatureExtension ext: es) {
				if(project.hasNature(ext.getName())) {
					nature = ext.getName();
					break;
				}
			}
		} catch (CoreException e) {
			ModelPlugin.getPluginLog().logError(e);
			return false;
		}
		return checkModelNature(project, nature);
	}

	public static boolean checkModelNature(IProject project, String nature) {
		if(project == null || !project.isOpen()) return false;
		if(nature == null) return false;
		String home = new ProjectHome().getLocation(project);
		if(home != null && home.length() > 0) {
			return true;
		}
		if(!project.isSynchronized(1)) {
			return false;
		}
		return true;
	}

}
