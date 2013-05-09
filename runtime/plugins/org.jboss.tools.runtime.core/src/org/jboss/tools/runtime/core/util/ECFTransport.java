package org.jboss.tools.runtime.core.util;

import java.io.OutputStream;
import java.net.ProtocolException;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ecf.core.security.IConnectContext;
import org.eclipse.ecf.filetransfer.UserCancelledException;
import org.jboss.tools.common.core.ecf.ECFTransportUtility;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;

/**
 * @author snjeza
 * based on org.eclipse.equinox.internal.p2.updatesite.ECFTransport
 * This class is only left here for legacy purposes. 
 * 
 * 
 * @Deprecated THIS CLASS SHOULD NOT BE USED, replaced by org.jboss.tools.common.core.ecf.ECFTransport
 */
@Deprecated
public class ECFTransport {

	/**
	 * Protected to allow backward compatability 
	 * from runtimes version, but avoid other instantiation
	 */
	protected ECFTransport() {
	}
	
	/**
	 * Returns an initialized instance of ECFExamplesTransport
	 */
	public static synchronized ECFTransport getInstance() {
		return new ECFTransport();
	}
	
	/**
	 * Gets the last modified date for the specified file.
	 * @param location - The URL location of the file.
	 * @return A <code>long</code> representing the date. Returns <code>0</code> if the file is not found or an error occurred.
	 * @exception OperationCanceledException if the request was canceled.
	 */
	public long getLastModified(URL location) throws CoreException {
		return new ECFTransportUtility().getLastModified(location);
	}
	
	/**
	 * Downloads the contents of the given URL to the given output stream. The
	 * destination stream will be closed by this method whether it succeeds
	 * to download or not.
	 */
	public IStatus download(String name, String url, OutputStream destination, IProgressMonitor monitor) {
		return new ECFTransportUtility().download(name, url, destination, monitor);
	}

	public IStatus performDownload(String name,String toDownload, OutputStream target, IConnectContext context, IProgressMonitor monitor) throws ProtocolException {
		return new ECFTransportUtility().performDownload(name, toDownload, target, context, monitor);
	}

	
	/**
	 * Returns the connection context for the given URL. This may prompt the
	 * user for user name and password as required.
	 * 
	 * @param xmlLocation - the file location requiring login details
	 * @param prompt - use <code>true</code> to prompt the user instead of
	 * looking at the secure preference store for login, use <code>false</code>
	 * to only try the secure preference store
	 * @throws UserCancelledException when the user cancels the login prompt 
	 * @throws CoreException if the password cannot be read or saved
	 * @return The connection context
	 */
	public IConnectContext getConnectionContext(String xmlLocation, boolean prompt) throws UserCancelledException, CoreException {
		return new ECFTransportUtility().getConnectionContext(xmlLocation, prompt);
	}
	
	/**
	 * This method should not be used and is unsafe. 
	 * It ungets the service object, thus making it useless, 
	 * before returning it. 
	 * 
	 * @param context
	 * @param name
	 * @return
	 * @deprecated
	 */
	@Deprecated
	public static Object getService(BundleContext context, String name) {
		if (context == null)
			return null;
		ServiceReference reference = context.getServiceReference(name);
		if (reference == null)
			return null;
		Object result = context.getService(reference);
		context.ungetService(reference);
		return result;
	}
}

