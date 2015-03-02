/******************************************************************************* 
 * Copyright (c) 2014 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.ui.util;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.jboss.tools.foundation.core.FoundationCorePlugin;
import org.jboss.tools.foundation.core.IURLProvider;
import org.jboss.tools.foundation.core.ecf.URLTransportUtility;
import org.jboss.tools.foundation.core.plugin.log.StatusFactory;
import org.jboss.tools.foundation.ui.internal.FoundationUIPlugin;

import sun.net.ProgressMonitor;

public class BrowserUtility {
	
	
	
	private static boolean browserLoadingErrorLoged;
	
	protected static Map<Integer, String> browserNames = new HashMap<Integer, String>(); 
	static {
		browserNames.put(SWT.WEBKIT, "Webkit");
		browserNames.put(SWT.MOZILLA, "Mozilla");
	}

	/**
	 * Opens a browser for the given url with the given id. If an error occurs
	 * it will be reported to the given log provider with the given plugin id.
	 * 
	 * @param url
	 *            the url to open a browser for.
	 * @param browserId
	 *            the id for the new browser.
	 * @param pluginId
	 *            the plugin id to log for.
	 * @param log
	 *            the log provider to log against if an error occurred.
	 */
	public static void checkedCreateInternalBrowser(String url, String browserId, String pluginId, ILog log) {
		try {
			openUrl(url, PlatformUI.getWorkbench().getBrowserSupport().createBrowser(IWorkbenchBrowserSupport.LOCATION_BAR | IWorkbenchBrowserSupport.NAVIGATION_BAR, browserId, null, null), pluginId, log);
		} catch (PartInitException e) {
			IStatus errorStatus = StatusFactory.errorStatus(pluginId, NLS.bind("Could not open browser for url \"{0}\".", url), e);
			log.log(errorStatus);
		}
	}
	
	public static void checkedCreateExternalBrowser(String url, String pluginId, ILog log) {
		try {
			openUrl(url, PlatformUI.getWorkbench().getBrowserSupport().getExternalBrowser(), pluginId, log);
		} catch (PartInitException e) {
			IStatus errorStatus = StatusFactory.errorStatus(pluginId, NLS.bind("Could not open browser for url \"{0}\".", url), e);
			log.log(errorStatus);
		}
	}

	public static void openUrl(String url, IWebBrowser browser, String pluginId, ILog log) {
		try {
			browser.openURL(new URL(url));
		} catch (PartInitException e) {
			IStatus errorStatus = StatusFactory.errorStatus(pluginId, NLS.bind("Could not open browser for url \"{0}\".", url), e);
			log.log(errorStatus);
		} catch (MalformedURLException e) {
			IStatus errorStatus = StatusFactory.errorStatus(pluginId, NLS.bind("Could not display malformed url \"{0}\".", url), e);
			log.log(errorStatus);
		}
	}
	
	static public void openExtenalBrowser(String URL) {
		checkedCreateExternalBrowser(URL,FoundationUIPlugin.PLUGIN_ID,FoundationUIPlugin.getDefault().getLog());
	}
	
	/**
	 * Creates an SWT Browser
	 * @param style
	 * @param parent
	 * @return
	 */
	
	public static Browser createBrowser(int style, Composite parent) {
		return createBrowser(style, parent, getPreferredBrowser());
	}

	/**
	 * Creates an SWT Browser
	 * @param parent
	 * @param preferredBrowser SWT.MOZILLA, SWT.WEBKIT or SWT.NONE
	 * @return
	 */
	public static Browser createBrowser(Composite parent) {
		return createBrowser(SWT.READ_ONLY | SWT.NO_SCROLL, parent, getPreferredBrowser());
	}
	
	/**
	 * Creates an SWT Browser
	 * @param parent
	 * @param preferredBrowser SWT.MOZILLA, SWT.WEBKIT or SWT.NONE
	 * @return
	 */
	public static Browser createBrowser(Composite parent, int preferredBrowser) {
		return createBrowser(SWT.READ_ONLY | SWT.NO_SCROLL, parent, preferredBrowser);
	}

	/**
	 * Creates an SWT Browser. Returns null if can't create a browser.
	 * @param style
	 * @param parent
	 * @param preferredBrowser SWT.MOZILLA, SWT.WEBKIT or SWT.NONE
	 * @return
	 * @see org.eclipse.swt.browser.Browser
	 */
	public static Browser createBrowser(int style, Composite parent, int preferredBrowser) {
		/*
			// We can provide webkit in this way:
			   String defaultBrowser = System.getProperty(Browser.PROPERTY_DEFAULTTYPE);
			   boolean hasDefaultBrowser = defaultBrowser != null;
			   System.getProperties().setProperty(Browser.PROPERTY_DEFAULTTYPE, "webkit");
		*/
		if(System.getProperty("skipBrowserCreation")!=null) {
			return null;
		}
		try {
			try {
				return new Browser(parent, style | preferredBrowser);
			} catch (SWTError e) {
				if(!(e instanceof SWTError) && !browserLoadingErrorLoged) {
					logBrowserLoadingProblem(e, browserNames.get(preferredBrowser), true);
				}
				Control[] children = parent.getChildren();
				for (Control child : children) {
					child.dispose();
				}
				try {
					return new Browser(parent, style | SWT.NONE);
				} catch (SWTError e1) {
					logBrowserLoadingProblem(e1, null, false);
					Control[] children1 = parent.getChildren();
					for (Control child : children1) {
						child.dispose();
					}
				}
			}
		} finally {
			/*
				//Use if system property was modified
				if(hasDefaultBrowser) {
					System.getProperties().setProperty(property, defaultBrowser);
				}
			*/
		}
		return null;
	}
	
	public static Control createBrowserOrLink(int style, Composite parent, int preferredBrowser, String url,
					String noBrowserText, Menu contextMenu) {
		Control result = createBrowserOrLink(style, parent, preferredBrowser, url, noBrowserText);
		result.setMenu(createLinkDefaultContextMenu(result, url));
		return result;
	}
	
	
	/**
	 * Return either a browser, or a link (if no browser can be created)
	 * 
	 * @param style
	 * @param parent
	 * @param preferredBrowser
	 * @param URL
	 * @param noBrowserText
	 * @return
	 */
	public static Control createBrowserOrLink(int style, Composite parent, int preferredBrowser, final String url,
					String noBrowserText) {
		return createBrowserOrLink(style, parent, preferredBrowser, new IURLProvider() {
			public String getUrl() {
				return url;
			}
		}, noBrowserText);
	}
	
	public static Control createBrowserOrLink(int style, Composite parent, int preferredBrowser, IURLProvider url,
			String noBrowserText) {

		Control result = null;
		Browser browser = createBrowser(style, parent, preferredBrowser);
		if (browser == null) {
			//TODO: Implement loading for .txt files and showing them in 
			// link as a text with visible hyperlink to open original address
			final Link link = new Link(parent, SWT.NONE);
			link.setText(noBrowserText);
			SelectionListener openExBrowser = new OpenExBrowserListener(url);
			link.addSelectionListener(openExBrowser);
			result = link;
		} else {
			result = browser;
		}
		return result;
	}

	private static void showProgressAndDownloadText(final Link link, final String URL) {
		final String msg = MessageFormat.format("Loading text file from {0}", URL);
		Job licenseDownloadJob = new Job("msg") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				URLTransportUtility transport = new URLTransportUtility();
				try {
					File propFile = transport.getCachedFileForURL(URL.toString(), msg,
									URLTransportUtility.CACHE_FOREVER, (int) 30000, 30000,
									new IProgressMonitor() {

						@Override
						public void worked(final int work) {
							link.getDisplay().asyncExec(new Runnable() {

								@Override
								public void run() {
									link.setText("Progress " + work);
								}
							});
						}

						@Override
						public void subTask(String name) {
							// TODO Auto-generated method stub

						}

						@Override
						public void setTaskName(String name) {
							// TODO Auto-generated method stub
						}

						@Override
						public void setCanceled(boolean value) {
							// TODO Auto-generated method stub
						}

						@Override
						public boolean isCanceled() {
							// TODO Auto-generated method stub
							return false;
						}

						@Override
						public void internalWorked(double work) {
							// TODO Auto-generated method stub
						}

						@Override
						public void done() {
							link.getDisplay().asyncExec(new Runnable() {
								@Override
								public void run() {
									// link
								}
							});
						}

						@Override
						public void beginTask(String name, int totalWork) {
							// TODO Auto-generated method stub
						}
					});
				} catch (CoreException e) {

				}

				return null;
			}
		};

	}

	private static Menu createLinkDefaultContextMenu (Control link, String url) {
		Menu popupMenu = new Menu(link);
	    MenuItem refreshItem = new MenuItem(popupMenu, SWT.NONE);
	    refreshItem.setText("Messages.DownloadRuntimeLicenseFragment_Open_in_external_browser");
	    refreshItem.addSelectionListener(new OpenExBrowserListener(url));
	    MenuItem copyToClipboard = new MenuItem(popupMenu, SWT.NONE);
	    copyToClipboard.setText("Messages.DownloadRuntimeLicenseFragment_Copy_URL_to_clipboard");
	    copyToClipboard.addSelectionListener(new CopyToClipboardListener(url));
	    link.setMenu(popupMenu);
	    return popupMenu;
	}
	
	private static void logBrowserLoadingProblem(Error e, String browserName, boolean warning) {
		if(browserName==null) {
			browserName = "default";
		}
		String message = "Cannot create " + browserName + " browser";
		if(warning) {
			FoundationUIPlugin.pluginLog().logWarning(message,e);
		} else {
			FoundationUIPlugin.pluginLog().logError(message, e);
		}
		browserLoadingErrorLoged = true;
	}

	protected static final boolean isMacOS = "Mac OS X".equals(System.getProperty("os.name"));

	public static int getPreferredBrowser() {
		return isMacOS ? SWT.WEBKIT : SWT.MOZILLA;
	}
	
	private static class OpenExBrowserListener extends SelectionAdapter {
		
		private String url;
		private IURLProvider provider;
		
		public OpenExBrowserListener(String url) {
			this.url = url;
		}
		
		public OpenExBrowserListener(IURLProvider provider) {
			this.provider = provider;
		}

		
		public void widgetSelected(SelectionEvent event) {
			if( url != null ) {
				BrowserUtility.openExtenalBrowser(url);
			} else {
				BrowserUtility.openExtenalBrowser(provider.getUrl());
			}
		}
	}
	
	private static class CopyToClipboardListener extends SelectionAdapter {
		
		private String url;
		private Clipboard cb = new Clipboard(Display.getCurrent()); 
		
		public CopyToClipboardListener(String url) {
			this.url = url;
		}
		
		@Override
		public void widgetSelected(SelectionEvent e) {
		    TextTransfer textTransfer = TextTransfer.getInstance();
		    cb.setContents(new Object[] { url },
		        new Transfer[] { textTransfer });
		}
	}
}
