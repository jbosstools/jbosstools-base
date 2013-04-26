package org.jboss.tools.stacks.core;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.BundleContext;

public class StacksCoreActivator extends Plugin {

	public static final String PLUGIN_ID = "org.jboss.tools.stacks.core";
	private static BundleContext context;
	private static StacksCoreActivator DEFAULT;
	
	public static StacksCoreActivator getDefault() {
		return DEFAULT;
	}
	
	public static BundleContext getBundleContext() {
		return context;
	}

	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext bundleContext) throws Exception {
		StacksCoreActivator.context = bundleContext;
		DEFAULT = this;
	}

	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext bundleContext) throws Exception {
		StacksCoreActivator.context = null;
	}
    public static void log(Throwable e) {
        IStatus status = new Status(IStatus.ERROR, PLUGIN_ID, e
                        .getLocalizedMessage(), e);
        getDefault().getLog().log(status);
    }

    public static void log(Throwable e, String message) {
        IStatus status = new Status(IStatus.ERROR, PLUGIN_ID, message, e);
        getDefault().getLog().log(status);
    }

	public static void log(String message) {
	        IStatus status = new Status(IStatus.INFO, PLUGIN_ID, message);
	        getDefault().getLog().log(status);
	}

}
