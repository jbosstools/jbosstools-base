package org.jboss.tools.runtime.core.model.internal;

import java.io.File;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.jboss.tools.foundation.core.tasks.TaskModel;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.core.model.IDownloadRuntimeWorkflowConstants;
import org.jboss.tools.runtime.core.model.IRuntimeInstaller;
import org.jboss.tools.runtime.core.util.internal.DownloadRuntimeOperationUtility;

public class BinaryInstaller implements IRuntimeInstaller {

	public BinaryInstaller() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public IStatus installRuntime(DownloadRuntime downloadRuntime, String unzipDirectory, String downloadDirectory,
			boolean deleteOnExit, TaskModel taskModel, IProgressMonitor monitor) {
		String user = (String)taskModel.getObject(IDownloadRuntimeWorkflowConstants.USERNAME_KEY);
		String pass = (String)taskModel.getObject(IDownloadRuntimeWorkflowConstants.PASSWORD_KEY);
		
		monitor.beginTask("Install Runtime '" + downloadRuntime.getName() + "' ...", 100);//$NON-NLS-1$ //$NON-NLS-2$
		monitor.worked(1);
		try {
			File f = new DownloadRuntimeOperationUtility().download(unzipDirectory, downloadDirectory, 
					getDownloadUrl(downloadRuntime, taskModel), deleteOnExit, user, pass, taskModel, new SubProgressMonitor(monitor, 80));
			File dest = new File(unzipDirectory, f.getName());
			f.renameTo(dest);
			dest.setExecutable(true);
			taskModel.putObject(IDownloadRuntimeWorkflowConstants.UNZIPPED_SERVER_HOME_DIRECTORY, unzipDirectory);
			taskModel.putObject(IDownloadRuntimeWorkflowConstants.UNZIPPED_SERVER_BIN, dest.getAbsolutePath());
		} catch(CoreException ce) {
			return ce.getStatus();
		}
		return Status.OK_STATUS;
	}

	private String getDownloadUrl(DownloadRuntime downloadRuntime, TaskModel taskModel) {
		if (downloadRuntime != null) {
			String dlUrl = downloadRuntime.getUrl();
			if (dlUrl == null) {
				return (String) taskModel.getObject(IDownloadRuntimeWorkflowConstants.DL_RUNTIME_URL);
			}
			return dlUrl;
		}
		return null;
	}

}
