package org.jboss.tools.runtime.ui.actions;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.project.examples.ProjectExamplesActivator;
import org.jboss.tools.project.examples.filetransfer.ECFExamplesTransport;
import org.jboss.tools.runtime.core.JBossRuntimeLocator;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.model.ServerDefinition;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;

public class DownloadRuntimeAction extends Action {

	private String runtimeId;
	
	public DownloadRuntimeAction(String runtimeId) {
		super();
		setRuntimeId(runtimeId);
	}

	private void setRuntimeId(String runtimeId) {
		Assert.isNotNull(runtimeId);
		this.runtimeId = runtimeId;
	}

	public DownloadRuntimeAction(String text, ImageDescriptor image, String runtimeId) {
		super(text, image);
		setRuntimeId(runtimeId);
	}

	public DownloadRuntimeAction(String text, int style, String runtimeId) {
		super(text, style);
		setRuntimeId(runtimeId);
	}

	public DownloadRuntimeAction(String text, String runtimeId) {
		super(text);
		setRuntimeId(runtimeId);
	}

	private void downloadRuntime(final DownloadRuntime runtime) {
		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		DirectoryDialog dialog = new DirectoryDialog(shell);
		dialog.setMessage("Select installation directory.");
		//dialog.setFilterPath("");
		final String selectedDirectory = dialog.open();
		if (selectedDirectory != null) {
			Job job = new Job("Download '" + runtime.getName() + "' ...") {
				
				@Override
				protected IStatus run(IProgressMonitor monitor) {
					return downloadAndInstall(runtime, selectedDirectory, monitor);
				}
			};
			job.setUser(true);
			job.schedule();
		}
	}
	
	private IStatus downloadAndInstall(DownloadRuntime runtime,
			String selectedDirectory, IProgressMonitor monitor) {
		FileInputStream in = null;
		OutputStream out = null;
		try {
			File file = File.createTempFile("JBossRuntime", "tmp");
			file.deleteOnExit();
			out = new BufferedOutputStream(
					new FileOutputStream(file));
			URL url = new URL(runtime.getUrl());
			String name = url.getPath();
			int slashIdx = name.lastIndexOf('/');
			if (slashIdx >= 0)
				name = name.substring(slashIdx + 1);
			
			IStatus result = ECFExamplesTransport.getInstance().download(name,
					url.toExternalForm(), out, monitor);
			out.flush();
			out.close();
			File directory = new File(selectedDirectory);
			directory.mkdirs();
			if (!directory.isDirectory()) {
				RuntimeCoreActivator.getDefault().getLog().log(result);
				// FIXME 
				return Status.CANCEL_STATUS;
			}
			ProjectExamplesActivator.extractZipFile(file, directory, monitor);
			if (!result.isOK()) {
				RuntimeCoreActivator.getDefault().getLog().log(result);
				// FIXME 
				return Status.CANCEL_STATUS;
			}
			createRuntimes(selectedDirectory, monitor);
		} catch (IOException e) {
			RuntimeCoreActivator.log(e);
			// FIXME 
		} finally {
			if (in != null) {
				try {
					in.close();
				} catch (IOException e) {
					// ignore
				}
			}
			if (out != null) {
				try {
					out.close();
				} catch (IOException e) {
					// ignore
				}
			}
		}
		return Status.OK_STATUS;
	}

	private static void createRuntimes(String directory, IProgressMonitor monitor) {
		JBossRuntimeLocator locator = new JBossRuntimeLocator();
		Set<RuntimePath> runtimePaths = RuntimeUIActivator.getDefault()
				.getRuntimePaths();
		RuntimePath newPath = new RuntimePath(directory);
		runtimePaths.add(newPath);
		for (RuntimePath runtimePath : runtimePaths) {
			List<ServerDefinition> serverDefinitions = locator
					.searchForRuntimes(runtimePath.getPath(),
							monitor);
			runtimePath.getServerDefinitions().clear();
			for (ServerDefinition serverDefinition : serverDefinitions) {
				serverDefinition.setRuntimePath(runtimePath);
			}
			runtimePath.getServerDefinitions().addAll(serverDefinitions);
		}
		List<ServerDefinition> serverDefinitions = RuntimeUIActivator
				.getDefault().getServerDefinitions();
		Set<IRuntimeDetector> detectors = RuntimeCoreActivator
				.getRuntimeDetectors();
		for (IRuntimeDetector detector : detectors) {
			if (detector.isEnabled()) {
				detector.initializeRuntimes(serverDefinitions);
			}
		}
	}

	@Override
	public void run() {
		Assert.isNotNull(runtimeId);
		DownloadRuntime runtime = RuntimeCoreActivator.getDefault().getDownloadJBossRuntimes().get(runtimeId);
		Assert.isNotNull(runtime);
		downloadRuntime(runtime);
	}

}
