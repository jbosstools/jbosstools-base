/*************************************************************************************
 * Copyright (c) 2018 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.launcher.ui.wizard;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.m2e.core.embedder.MavenModelManager;
import org.eclipse.m2e.core.internal.MavenPluginActivator;
import org.eclipse.m2e.core.project.IProjectConfigurationManager;
import org.eclipse.m2e.core.project.LocalProjectScanner;
import org.eclipse.m2e.core.project.MavenProjectInfo;
import org.eclipse.m2e.core.project.ProjectImportConfiguration;
import org.jboss.tools.common.launcher.core.LauncherCorePlugin;
import org.jboss.tools.common.launcher.core.model.CatalogManager;
import org.jboss.tools.common.launcher.ui.LauncherUIPlugin;

public class NewLauncherProjectWizardController {

	private static final String JS_PACKAGE_JSON_FILE = "package.json";
	private static final String MAVEN_POM_XML_FILE = "pom.xml";
	private NewLauncherProjectModel model;

	public NewLauncherProjectWizardController(NewLauncherProjectModel model) {
		this.model = model;
	}
	
	public IStatus run(IProgressMonitor monitor) {
		IStatus status = getZip(monitor);
		if (!monitor.isCanceled() 
				&& status.isOK()) {
			status = createProject(monitor);
		}
		return status;
	}

	private IStatus getZip(IProgressMonitor monitor) {
		CatalogManager manager = CatalogManager.getDefault();
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		IStatus status = null;
		try {
			status =  manager.zip(LauncherCorePlugin.getDefault().getDefaultEndpointURL(), 
					model.getSelectedBooster().getMission(), 
					model.getSelectedBooster().getRuntime(), 
					model.getSelectedBooster().getVersion(), 
					model.getProjectName(), 
					model.getGroupId(), 
					model.getArtifactId(), 
					model.getVersion(), 
					output, monitor);
			if (!monitor.isCanceled() 
					&& status.isOK()) {
				status = unzip(output.toByteArray(), model.getLocation());
			}
		} finally {
			//NOCODE
		}
		return status;
	}

	private IStatus createProject(IProgressMonitor monitor) {
		if (isMavenProject()) {
			return createMavenProject(monitor);
		} else if (isJavascriptProject()) {
			return createJavascriptProject(monitor);
		} else {
			return createGeneralProject(monitor);
		}
	}


	private IStatus createGeneralProject(IProgressMonitor monitor) {
		IStatus status = Status.OK_STATUS;
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		try {
			IProjectDescription description = workspace.newProjectDescription(model.getProjectName());
			description.setLocation(model.getLocation());
			IProject project = workspace.getRoot().getProject(model.getProjectName());
			project.create(description, monitor);
			project.open(IResource.BACKGROUND_REFRESH, monitor);
		} catch (CoreException e) {
			status = e.getStatus();
		}
		return status;
	}

	private IStatus createJavascriptProject(IProgressMonitor monitor) {
		return createGeneralProject(monitor);
	}

	private IStatus createMavenProject(IProgressMonitor monitor) {
		IStatus status = Status.OK_STATUS;
		MavenPluginActivator mavenPlugin = MavenPluginActivator.getDefault();
		IProjectConfigurationManager configurationManager = mavenPlugin.getProjectConfigurationManager();
		MavenModelManager modelManager = mavenPlugin.getMavenModelManager();
		LocalProjectScanner scanner = new LocalProjectScanner(
				ResourcesPlugin.getWorkspace().getRoot().getRawLocation().toFile(), 
				model.getLocation().toOSString(), 
				false,
				modelManager);
		try {
			scanner.run(monitor);
			ProjectImportConfiguration projectImportConfiguration = new ProjectImportConfiguration();
			configurationManager.importProjects(collect(scanner.getProjects()), projectImportConfiguration, monitor);
		} catch (InterruptedException e) {
			status = new Status(IStatus.ERROR, LauncherUIPlugin.PLUGIN_ID, e.getLocalizedMessage(), e); 
		} catch (CoreException e) {
			status = e.getStatus();
		}
		return status;
	}

	private boolean isMavenProject() {
		return model.getLocation().append(MAVEN_POM_XML_FILE).toFile().exists();
	}

	private boolean isJavascriptProject() {
		return model.getLocation().append(JS_PACKAGE_JSON_FILE).toFile().exists();
	}
	
	private void unzipEntry(IPath location, ZipInputStream stream, ZipEntry entry)
			throws IOException {
		try {
			String name = skipOneLevel(entry.getName());
			if (!name.isEmpty()) {
				Path path = Paths.get(location.toOSString(), name);
				if (entry.isDirectory()) {
					Files.createDirectories(path);
				} else {
					try (OutputStream output = new FileOutputStream(path.toFile())) {
						IOUtils.copy(stream, output);
					}
				}
			}
		} catch (InvalidPathException e) {
			LauncherUIPlugin.getDefault().getLog().log(new Status(IStatus.WARNING, LauncherUIPlugin.PLUGIN_ID, e.getLocalizedMessage(), e));
		}
	}

	private IStatus unzip(byte[] byteArray, IPath location) {
		ZipInputStream stream = new ZipInputStream(new ByteArrayInputStream(byteArray));
		ZipEntry entry;
		IStatus status = Status.OK_STATUS;
		try {
			while ((entry = stream.getNextEntry()) != null) {
				unzipEntry(location, stream, entry);
			}
		} catch (IOException e) {
			status = new Status(IStatus.ERROR, LauncherUIPlugin.PLUGIN_ID, e.getLocalizedMessage(), e);
		}
		return status;
	}

	
	/**
	 * Skip first level in path because Launcher backend returns a zip where the first folder is
	 * the artifact id.
	 * 
	 * @param path the path of the zip entry
	 * @return the computed path
	 */
	private String skipOneLevel(String path) {
		int index = path.indexOf('/');
		if (index != (-1)) {
			path = path.substring(index+1);
		}
		return path;
	}
	
	private Set<MavenProjectInfo> collect(Collection<MavenProjectInfo> projects) {
		return new LinkedHashSet<MavenProjectInfo>() {
			private static final long serialVersionUID = 1L;

			public Set<MavenProjectInfo> collect(Collection<MavenProjectInfo> projects) {
				for (MavenProjectInfo projectInfo : projects) {
					add(projectInfo);
					collect(projectInfo.getProjects());
				}
				return this;
			}
		}.collect(projects);
	}

}
