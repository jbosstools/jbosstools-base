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

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.MessageDialogWithToggle;
import org.eclipse.jface.preference.IPreferenceStore;
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
import org.eclipse.ui.preferences.ScopedPreferenceStore;
import org.jboss.tools.foundation.core.IURLProvider;
import org.jboss.tools.foundation.core.plugin.log.StatusFactory;
import org.jboss.tools.foundation.ui.internal.FoundationUIPlugin;


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
	public void checkedCreateInternalBrowser(String url, String browserId, String pluginId, ILog log) {
		try {
			openUrl(url, PlatformUI.getWorkbench().getBrowserSupport().createBrowser(IWorkbenchBrowserSupport.LOCATION_BAR | IWorkbenchBrowserSupport.NAVIGATION_BAR, browserId, null, null), pluginId, log);
		} catch (PartInitException e) {
			IStatus errorStatus = StatusFactory.errorStatus(pluginId, NLS.bind("Could not open browser for url \"{0}\".", url), e);
			log.log(errorStatus);
		}
	}
	
	public void checkedCreateExternalBrowser(String url, String pluginId, ILog log) {
		try {
			openUrl(url, PlatformUI.getWorkbench().getBrowserSupport().getExternalBrowser(), pluginId, log);
		} catch (PartInitException e) {
			IStatus errorStatus = StatusFactory.errorStatus(pluginId, NLS.bind("Could not open browser for url \"{0}\".", url), e);
			log.log(errorStatus);
		}
	}

	public void openUrl(String url, IWebBrowser browser, String pluginId, ILog log) {
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
	
	/**
	 * @deprecated
	 * Typo in method name.
	 * <p> Use {@link #openExternalBrowser(String URL)} instead.
	 */
	@Deprecated
	public void openExtenalBrowser(String URL) {
		openExternalBrowser(URL);
	}
	
	public void openExternalBrowser(String URL) {
		checkedCreateExternalBrowser(URL,FoundationUIPlugin.PLUGIN_ID,FoundationUIPlugin.getDefault().getLog());
	}
	
	/**
	 * Creates an SWT Browser
	 * @param style
	 * @param parent
	 * @return
	 */
	
	public Browser createBrowser(int style, Composite parent) {
		return createBrowser(style, parent, getPreferredBrowser());
	}

	/**
	 * Creates an SWT Browser
	 * @param parent
	 * @param preferredBrowser SWT.MOZILLA, SWT.WEBKIT or SWT.NONE
	 * @return
	 */
	public Browser createBrowser(Composite parent) {
		return createBrowser(SWT.READ_ONLY | SWT.NO_SCROLL, parent, getPreferredBrowser());
	}
	
	/**
	 * Creates an SWT Browser
	 * @param parent
	 * @param preferredBrowser SWT.MOZILLA, SWT.WEBKIT or SWT.NONE
	 * @return
	 */
	public Browser createBrowser(Composite parent, int preferredBrowser) {
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
	public Browser createBrowser(int style, Composite parent, int preferredBrowser) {
		return createBrowser(style, parent, preferredBrowser, true);
	}
	
	private Browser createBrowser(int style, Composite parent, int preferredBrowser, boolean logError) {
		/*
			// We can provide webkit in this way:
			   String defaultBrowser = System.getProperty(Browser.PROPERTY_DEFAULTTYPE);
			   boolean hasDefaultBrowser = defaultBrowser != null;
			   System.getProperties().setProperty(Browser.PROPERTY_DEFAULTTYPE, "webkit");
		*/
		String val = System.getProperty("jbosstools.skip.browser.creation"); 
		if(val!=null) {
			boolean skip = Boolean.parseBoolean(val);
			if( skip )
				return null;
		}
		try {
			try {
				return new Browser(parent, style | preferredBrowser);
			} catch (SWTError e) {
				boolean preLogged = browserLoadingErrorLoged;
				if(!preLogged && logError) {
					logBrowserLoadingProblem(e, browserNames.get(preferredBrowser), true);
				}
				Control[] children = parent.getChildren();
				for (Control child : children) {
					child.dispose();
				}
				try {
					return new Browser(parent, style | SWT.NONE);
				} catch (SWTError e1) {
					if( !preLogged && logError )
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
	
	public Control createBrowserOrLink(int style, Composite parent, int preferredBrowser, String url,
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
	public Control createBrowserOrLink(int style, Composite parent, int preferredBrowser, final String url,
					String noBrowserText) {
		return createBrowserOrLink(style, parent, preferredBrowser, new IURLProvider() {
			public String getUrl() {
				return url;
			}
		}, noBrowserText);
	}
	
	public Control createBrowserOrLink(int style, Composite parent, IURLProvider url, String noBrowserText) {
		return createBrowserOrLink(style, parent, getPreferredBrowser(), url, noBrowserText);
	}
	
	public Control createBrowserOrLink(int style, Composite parent, int preferredBrowser, IURLProvider url,
			String noBrowserText) {

		Control result = null;
		Browser browser = createBrowser(style, parent, preferredBrowser, false);
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

	private Menu createLinkDefaultContextMenu (Control link, String url) {
		Menu popupMenu = new Menu(link);
	    MenuItem refreshItem = new MenuItem(popupMenu, SWT.NONE);
	    refreshItem.setText(BrowserUtilityMessages.Open_in_external_browser);
	    refreshItem.addSelectionListener(new OpenExBrowserListener(url));
	    MenuItem copyToClipboard = new MenuItem(popupMenu, SWT.NONE);
	    copyToClipboard.setText(BrowserUtilityMessages.Copy_URL_to_clipboard);
	    copyToClipboard.addSelectionListener(new CopyToClipboardListener(url));
	    link.setMenu(popupMenu);
	    return popupMenu;
	}
	
	private static void logBrowserLoadingProblem(Error e, String browserName, boolean warning) {
		Exception e2 = new Exception(e);
		if(browserName==null) {
			browserName = "default";
		}
		String message = "Cannot create " + browserName + " browser";
		if(warning) {
			FoundationUIPlugin.pluginLog().logWarning(message,e2);
		} else {
			FoundationUIPlugin.pluginLog().logError(message, e2);
		}
		browserLoadingErrorLoged = true;
	}

	protected static final boolean isMacOS = "Mac OS X".equals(System.getProperty("os.name"));

	private static final String PREFS_ALLOW_NON_HTTPS = "org.jboss.tools.foundation.ui.util.ALLOW_NON_HTTPS";

	public static int getPreferredBrowser() {
		return isMacOS ? SWT.WEBKIT : SWT.MOZILLA;
	}
	
	public class OpenExBrowserListener extends SelectionAdapter {
		
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
				BrowserUtility.this.openExtenalBrowser(url);
			} else {
				BrowserUtility.this.openExtenalBrowser(provider.getUrl());
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

	  /**
	   * Set the transport policy on MacOS to allow redirects to non-https urls.
	   * 
	   * @see <a href="https://bugs.eclipse.org/bugs/show_bug.cgi?id=568749">Eclipse bug 568749</a>
	   */
	  /**
	 * 
	 */
	public static void allowNonHttpsRedirects() {
		if (!Platform.OS_MACOSX.equals(Platform.getOS())) {
			return;
		}
		AppleTransportSecurity security = new AppleTransportSecurity();
		if (security.isHttpAllowed("localhost")) {
			return;
		}

		if (!confirmAllowNonHttpsRedirects()) {
			return;
		}
		security.allowHttp("localhost");
	  }

	private static boolean confirmAllowNonHttpsRedirects() {
		IPreferenceStore store = new ScopedPreferenceStore(InstanceScope.INSTANCE, FoundationUIPlugin.PLUGIN_ID);
		if (showAllowNonHttpsRedirectsDialog(store)) {
			MessageDialogWithToggle dialog = openAllowNonHttpsRedirectsDialog(store);
			return saveAllowNonHttpsRedirects(dialog, store);
		} else {
			return getAllowNonHttpsRedirects(store);
		}		
	}
	
	private static MessageDialogWithToggle openAllowNonHttpsRedirectsDialog(IPreferenceStore store) {
		MessageDialogWithToggle dialog = new MessageDialogWithToggle(
				Display.getCurrent().getActiveShell(),
				"Allow Non-Https Connections?",
				null, // accept the default window icon
				"Non-Https connection required. Add a temporary exception for localhost to allow it?\n"
				+ "If not granted, connections will fail.",
				MessageDialog.QUESTION,
				new String[] { IDialogConstants.YES_LABEL, IDialogConstants.NO_LABEL },
				0,
				"Don't ask again",
				false) {

			@Override
			protected Control createCustomArea(Composite parent) {
				Link link = new Link(parent, SWT.None);
				link.setText("For custom Eclipse IDEs manually changing Info.plist within the application bundle is needed. See <a>JBIDE-28007</a>.\n");
				link.addSelectionListener(new SelectionAdapter() {

					@Override
					public void widgetSelected(SelectionEvent e) {
						 new BrowserUtility().openExtenalBrowser("https://issues.redhat.com/browse/JBIDE-28007?focusedCommentId=19060991&page=com.atlassian.jira.plugin.system.issuetabpanels:comment-tabpanel#comment-19060991");
					}
				});
				return link;
			}

		};
		dialog.setPrefStore(store);
		dialog.setPrefKey(PREFS_ALLOW_NON_HTTPS);
		dialog.open();
		return dialog;
	}

	private static boolean getAllowNonHttpsRedirects(IPreferenceStore store) {
		String allow = store.getString(PREFS_ALLOW_NON_HTTPS);
		return MessageDialogWithToggle.ALWAYS == allow 
				|| MessageDialogWithToggle.NEVER == allow;
	}

	private static boolean showAllowNonHttpsRedirectsDialog(IPreferenceStore store) {
		return !store.contains(PREFS_ALLOW_NON_HTTPS);
	}

	private static boolean saveAllowNonHttpsRedirects(MessageDialogWithToggle dialog, IPreferenceStore store) {
		if (dialog.getToggleState()) {
			String value = null;
			if (dialog.getReturnCode() == IDialogConstants.YES_ID) {
				value = MessageDialogWithToggle.ALWAYS;
			} else if (dialog.getReturnCode() == IDialogConstants.NO_ID) {
				value = MessageDialogWithToggle.NEVER;
			}
			store.setValue(PREFS_ALLOW_NON_HTTPS, value);
		} 
		return dialog.getReturnCode() == IDialogConstants.YES_ID;
	}
}
