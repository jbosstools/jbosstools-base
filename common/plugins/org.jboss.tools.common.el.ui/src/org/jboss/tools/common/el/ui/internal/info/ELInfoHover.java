/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.el.ui.internal.info;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.internal.ui.actions.OpenBrowserUtil;
import org.eclipse.jdt.internal.ui.actions.SimpleSelectionProvider;
import org.eclipse.jdt.internal.ui.infoviews.JavadocView;
import org.eclipse.jdt.internal.ui.text.java.hover.JavadocBrowserInformationControlInput;
import org.eclipse.jdt.internal.ui.text.java.hover.JavadocHover;
import org.eclipse.jdt.internal.ui.text.javadoc.JavadocContentAccess2;
import org.eclipse.jdt.internal.ui.viewsupport.BasicElementLabels;
import org.eclipse.jdt.internal.ui.viewsupport.JavaElementLinks;
import org.eclipse.jdt.ui.JavaElementLabels;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jdt.ui.actions.OpenAttachedJavadocAction;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.internal.text.html.BrowserInformationControl;
import org.eclipse.jface.internal.text.html.BrowserInformationControlInput;
import org.eclipse.jface.internal.text.html.BrowserInput;
import org.eclipse.jface.internal.text.html.HTMLPrinter;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.AbstractReusableInformationControlCreator;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IInformationControlExtension4;
import org.eclipse.jface.text.IInputChangedListener;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.LocationEvent;
import org.eclipse.swt.browser.LocationListener;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.wst.xml.ui.internal.taginfo.XMLTagInfoHoverProcessor;
import org.jboss.tools.common.el.ui.ElUiPlugin;
import org.jboss.tools.common.el.ui.ca.ELProposalProcessor;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.JarSystemImpl;
import org.osgi.framework.Bundle;

/**
 * A utility class for Hover Info and Content Assist Additional Info creation
 * 
 * @author Victor V. Rubezhny
 *
 */
@SuppressWarnings("restriction")
public class ELInfoHover extends XMLTagInfoHoverProcessor {
	
	private IInformationControlCreator fHoverControlCreator;
	private IInformationControlCreator fPresenterControlCreator;

	/*
	 * @see org.eclipse.jdt.internal.ui.text.java.hover.AbstractJavaEditorTextHover#getInformationPresenterControlCreator()
	 */
	public IInformationControlCreator getInformationPresenterControlCreator() {
		final boolean[] browserInformationControlAvailable = new boolean[] {false};
		Display.getDefault().syncExec(new Runnable() {
			@Override
			public void run() {
				Shell shell= JavaPlugin.getActiveWorkbenchShell();
				browserInformationControlAvailable[0] = (shell != null && BrowserInformationControl.isAvailable(shell));
			}
		});
		
		if (!browserInformationControlAvailable[0])
			return null;
		
		if (fPresenterControlCreator == null)
			fPresenterControlCreator= new PresenterControlCreator(getSite());
		return fPresenterControlCreator;
	}
	/*
	 * @see ITextHoverExtension#getHoverControlCreator()
	 */
	@Override
	public IInformationControlCreator getHoverControlCreator() {
		if (fHoverControlCreator == null)
			fHoverControlCreator= new HoverControlCreator(getInformationPresenterControlCreator(), null);
		return fHoverControlCreator;
	}
	
	public static Object getHoverInfo(String info, IProgressMonitor monitor) {
		if (info != null && info.length() > 0) {
			StringBuffer buffer= new StringBuffer();
			HTMLPrinter.insertPageProlog(buffer, 0, getCSSStyles());
			buffer.append(info.trim());
			HTMLPrinter.addPageEpilog(buffer);
			
			return new ELBrowserInformationControlInput(null, null, processLinks(buffer.toString()), 0);
		}
		return null;
	}
	
	public static Object getHoverInfo(IJavaElement[] javaElements, IProgressMonitor monitor) {
		if (javaElements != null && javaElements.length > 0) {
			Arrays.sort(javaElements, ELProposalProcessor.CASE_INSENSITIVE_ORDER);
			return new ELBrowserInformationControlInput(null, javaElements, 
					processLinks(extractProposalContextInfo(javaElements)), 0);
		}
		return null;
	}

	public static Object getHoverInfo(String baseName, String propertyName, Object allObjects,  IProgressMonitor monitor) {
		String info = extractProposalContextInfo(baseName, propertyName, allObjects);
		return info != null ? new ELBrowserInformationControlInput(null, null, processLinks(info), 0) : null;
	}

	public static IWorkbenchSite getSite() {
		IWorkbench workBench = ElUiPlugin.getDefault() == null ? null : ElUiPlugin.getDefault().getWorkbench();
		IWorkbenchWindow window = workBench == null ? null : workBench.getActiveWorkbenchWindow();
		IWorkbenchPage page = window == null ? null : window.getActivePage();
		IWorkbenchPart part = page == null ? null : page.getActivePart();
		return part == null ? null : part.getSite();
	}
	
	/*
	 * Extracts the additional proposal information based on Javadoc for the
	 * stored IJavaElement objects
	 */
	private static String extractProposalContextInfo(IJavaElement[] elements) {
		int nResults = elements.length;
		StringBuffer buffer = new StringBuffer();
		boolean hasContents = false;
		IJavaElement element = null;

		if (nResults > 1) {
			for (int i = 0; i < elements.length; i++) {
				if (elements[i] == null)
					continue;
				if (elements[i] instanceof IMember
						|| elements[i].getElementType() == IJavaElement.LOCAL_VARIABLE
						|| elements[i].getElementType() == IJavaElement.TYPE_PARAMETER) {
					addFullInfo(buffer, elements[i]);
					buffer.append("<br/>"); //$NON-NLS-1$
					hasContents = true;
				}
			}
		} else {
			element = elements[0];
			if (element instanceof IMember
					|| element.getElementType() == IJavaElement.LOCAL_VARIABLE
					|| element.getElementType() == IJavaElement.TYPE_PARAMETER) {
				addFullInfo(buffer, element);
				hasContents = true;
			}
		}

		if (!hasContents || buffer.length() == 0)
			return null;

		HTMLPrinter.insertPageProlog(buffer, 0, getCSSStyles());
		HTMLPrinter.addPageEpilog(buffer);
		return buffer.toString();
	}

	/*
	 * Adds full information to the additional proposal information
	 * 
	 * @param buffer
	 * 
	 * @param element
	 * 
	 * @return
	 */
	private static void addFullInfo(StringBuffer buffer, IJavaElement element) {
		if (element instanceof IMember) {
			IMember member = (IMember) element;
			HTMLPrinter.addSmallHeader(buffer, getInfoText(member));
			Reader reader = null;
			try {
				String content = JavadocContentAccess2.getHTMLContent(member,
						true);
				reader = content == null ? null : new StringReader(content);
			} catch (JavaModelException ex) {
				ElUiPlugin.getDefault().logError(ex);
			} catch (CoreException e) {
				ElUiPlugin.getDefault().logError(e);
			}

			if (reader == null) {
				reader = new StringReader(Messages.NO_JAVADOC);
			}

			if (reader != null) {
				buffer.append("<br/>"); //$NON-NLS-1$
				buffer.append(HTMLPrinter.read(reader));
			}

		} else if (element.getElementType() == IJavaElement.LOCAL_VARIABLE
				|| element.getElementType() == IJavaElement.TYPE_PARAMETER) {
			HTMLPrinter.addSmallHeader(buffer, getInfoText(element));
		}
	}
	
	private static final long LABEL_FLAGS = JavaElementLabels.ALL_FULLY_QUALIFIED
			| JavaElementLabels.M_PRE_RETURNTYPE
			| JavaElementLabels.M_PARAMETER_TYPES
			| JavaElementLabels.M_PARAMETER_NAMES
			| JavaElementLabels.M_EXCEPTIONS
			| JavaElementLabels.F_PRE_TYPE_SIGNATURE
			| JavaElementLabels.M_PRE_TYPE_PARAMETERS
			| JavaElementLabels.T_TYPE_PARAMETERS
			| JavaElementLabels.USE_RESOLVED;
	private static final long LOCAL_VARIABLE_FLAGS = LABEL_FLAGS
			& ~JavaElementLabels.F_FULLY_QUALIFIED
			| JavaElementLabels.F_POST_QUALIFIED;
	private static final long TYPE_PARAMETER_FLAGS = LABEL_FLAGS
			| JavaElementLabels.TP_POST_QUALIFIED;

	/*
	 * Returns the label for the IJavaElement objects
	 */
	private static String getInfoText(IJavaElement element) {
		long flags;
		switch (element.getElementType()) {
		case IJavaElement.LOCAL_VARIABLE:
			flags = LOCAL_VARIABLE_FLAGS;
			break;
		case IJavaElement.TYPE_PARAMETER:
			flags = TYPE_PARAMETER_FLAGS;
			break;
		default:
			flags = LABEL_FLAGS;
			break;
		}

		return JavadocHover.getImageAndLabel(element, true, JavaElementLinks.getElementLabel(element, flags, true));
	}

	/*
	 * Extracts the additional proposal information based on Javadoc for the
	 * stored IJavaElement objects
	 */
	private static String extractProposalContextInfo(
			String baseName, String propertyName, Object allObjects) {
		Assert.isTrue(allObjects instanceof List<?>);
		
		String info = getELMessagesHoverInternal(
				baseName, propertyName, (List<XModelObject>) allObjects);
		
		if (info == null) return null;
		
		StringBuffer buffer = new StringBuffer();
		buffer.append(info);

		if (buffer.length() == 0) return null;

		HTMLPrinter.insertPageProlog(buffer, 0, getCSSStyles());
		HTMLPrinter.addPageEpilog(buffer);
		return buffer.toString();
	}

	public static String getELMessagesHoverInternal(String baseName, String propertyName, List<XModelObject> objects) {
		StringBuffer buffer= new StringBuffer();

		if (propertyName != null && propertyName.length() > 0) 
			buffer.append(MessageFormat.format(Messages.ELInfoHover_propertyName, 
				propertyName));
			
		if (baseName != null && baseName.length() > 0)
			buffer.append(MessageFormat.format(Messages.ELInfoHover_baseName, 
					baseName));
		
		if (objects != null) {
			boolean firstValue = true;
			for (XModelObject o : objects) {
				IFile propFile = (IFile)o.getAdapter(IFile.class);
				String propFilePath = null;
				if (propFile != null) {
					propFilePath = propFile.getFullPath().toString();
				} else {
					XModelObject parent = o.getFileType() == XModelObject.FILE ? o : o.getParent();
					String path = parent.getPath();
					while (parent != null && parent.getFileType() != XModelObject.SYSTEM) {
						parent = parent.getParent();
					}
					if (parent instanceof JarSystemImpl) {
						String sysPath = parent.getPath();
						path = path.substring(sysPath.length());
	
						String jarPath = ((JarSystemImpl) parent).getLocation();
						
						IResource jar = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(new Path(jarPath));
						
						if (jar != null) {
							jarPath = jar.getFullPath().toString();
						}
						
						propFilePath = jarPath + "!" + path; //$NON-NLS-1$
					}
				}
				buffer.append(Messages.ELInfoHover_newLine);
				if (!firstValue) buffer.append(Messages.ELInfoHover_newLine);
				else firstValue = false;
				
				buffer.append(MessageFormat.format(Messages.ELInfoHover_resourceBundle, 
						propFilePath != null ? propFilePath : Messages.ELInfoHover_resourceBundleNotDefined));
				
				if (propertyName != null) {
					String value = o.get("VALUE");  //$NON-NLS-1$
					boolean addCut = false;
					if (value != null) {
						if (value.length() > 100) {
							// Get first words of value
							int lastSpace = value.lastIndexOf(' ', 99);
							if (lastSpace != -1) {
								value = value.substring(0, lastSpace);
							} else { // cut as is
								value = value.substring(0, 100);
							}
							addCut = true;
						}
					}
					buffer.append(Messages.ELInfoHover_newLine);
					buffer.append(MessageFormat.format(Messages.ELInfoHover_resourceBundlePropertyValue, 
							value != null ? value : Messages.ELInfoHover_resourceBundlePropertyValueNotDefined,
									addCut ? Messages.ELInfoHover_treeDots : "")); //$NON-NLS-1$
				}
			}
		}

		return buffer.length() == 0 ? null : buffer.toString();
	}

	
	private static final String[] PROTOCOLS = {
		JavaElementLinks.JAVADOC_VIEW_SCHEME,
		JavaElementLinks.JAVADOC_SCHEME,
		JavaElementLinks.OPEN_LINK_SCHEME,
		"http", //$NON-NLS-1$
		"https" //$NON-NLS-1$
	};
	
	protected static String processLinks(String html) {
		if (html == null)
			return ""; //$NON-NLS-1$
		String text;
		try {
			HTMLAnchorProcessor reader= new HTMLAnchorProcessor(new StringReader(html), PROTOCOLS);
			text= reader.getString();
			reader.close();
		} catch (IOException e) {
			text= ""; //$NON-NLS-1$
		}
		return text;
	}

	private static String fCSSStyles;
	
	/**
	 * Returns the style information for displaying HTML content.
	 *
	 * @return the CSS styles
	 */
	private static String getCSSStyles() {
		if (fCSSStyles == null) {
			Bundle bundle= Platform.getBundle(ElUiPlugin.PLUGIN_ID);
			URL url= bundle.getEntry("/resources/css/ELInfoHoverStyleSheet.css"); //$NON-NLS-1$
			if (url != null) {
				BufferedReader reader= null;
				try {
					url= FileLocator.toFileURL(url);
					reader= new BufferedReader(new InputStreamReader(url.openStream()));
					StringBuilder buffer= new StringBuilder(200);
					String line= reader.readLine();
					while (line != null) {
						buffer.append(line);
						buffer.append('\n');
						line= reader.readLine();
					}
					fCSSStyles= buffer.toString();
				} catch (IOException ex) {
					ElUiPlugin.getDefault().logError(ex);
				} finally {
					try {
						if (reader != null)
							reader.close();
					} catch (IOException e) {
					}
				}

			}
		}
		String css= fCSSStyles;
		if (css != null) {
			FontData fontData= JFaceResources.getFontRegistry().getFontData(JFaceResources.DIALOG_FONT)[0];
			css= HTMLPrinter.convertTopLevelFont(css, fontData);
		}
		return css;
	}

	
	/**
	 * Hover control creator.
	 */
	public static final class HoverControlCreator extends AbstractReusableInformationControlCreator {
		/**
		 * The information presenter control creator.
		 */
		private final IInformationControlCreator fInformationPresenterControlCreator;
		private String fAffordance = null;

		/**
		 * @param informationPresenterControlCreator control creator for enriched hover
		 */
		public HoverControlCreator(IInformationControlCreator informationPresenterControlCreator, String affordance) {
			fInformationPresenterControlCreator= informationPresenterControlCreator;
			fAffordance = affordance;
		}

		/*
		 * @see org.eclipse.jdt.internal.ui.text.java.hover.AbstractReusableInformationControlCreator#doCreateInformationControl(org.eclipse.swt.widgets.Shell)
		 */
		@Override
		public IInformationControl doCreateInformationControl(Shell parent) {
			if (BrowserInformationControl.isAvailable(parent)) {
				BrowserInformationControl iControl = new BrowserInformationControl(parent, null, 
								fAffordance == null ? Messages.hover_affordance : fAffordance) {
					/*
					 * @see org.eclipse.jface.text.IInformationControlExtension5#getInformationPresenterControlCreator()
					 */
					@Override
					public IInformationControlCreator getInformationPresenterControlCreator() {
						return fInformationPresenterControlCreator;
					}
				};
				addLinkListener(iControl);
				return iControl;
			} else {
				return new DefaultInformationControl(parent, fAffordance == null ? Messages.hover_affordance : fAffordance);
			}
		}

		/*
		 * @see org.eclipse.jdt.internal.ui.text.java.hover.AbstractReusableInformationControlCreator#canReuse(org.eclipse.jface.text.IInformationControl)
		 */
		@Override
		public boolean canReuse(IInformationControl control) {
			if (!super.canReuse(control))
				return false;

			if (control instanceof IInformationControlExtension4) {
				((IInformationControlExtension4)control).setStatusText(fAffordance == null ? Messages.hover_affordance : fAffordance);
			}

			return true;
		}
	}

	
	/**
	 * Presenter control creator.
	 */
	public static final class PresenterControlCreator extends AbstractReusableInformationControlCreator {
		private final IWorkbenchSite fSite;

		/**
		 * Creates a new PresenterControlCreator.
		 * 
		 * @param site the site or <code>null</code> if none
		 */
		public PresenterControlCreator(IWorkbenchSite site) {
			fSite= site;
		}

		/*
		 * @see org.eclipse.jdt.internal.ui.text.java.hover.AbstractReusableInformationControlCreator#doCreateInformationControl(org.eclipse.swt.widgets.Shell)
		 */
		@Override
		public IInformationControl doCreateInformationControl(Shell parent) {
			if (BrowserInformationControl.isAvailable(parent)) {
				ToolBarManager tbm= new ToolBarManager(SWT.FLAT);
				BrowserInformationControl iControl= new BrowserInformationControl(parent, null, tbm);

				final BackAction backAction= new BackAction(iControl);
				backAction.setEnabled(false);
				tbm.add(backAction);
				final ForwardAction forwardAction= new ForwardAction(iControl);
				tbm.add(forwardAction);
				forwardAction.setEnabled(false);

				final ShowInJavadocViewAction showInJavadocViewAction= new ShowInJavadocViewAction(iControl);
				tbm.add(showInJavadocViewAction);
				final OpenDeclarationAction openDeclarationAction= new OpenDeclarationAction(iControl);
				tbm.add(openDeclarationAction);

				final SimpleSelectionProvider selectionProvider= new SimpleSelectionProvider();
				if (fSite != null) {
					OpenAttachedJavadocAction openAttachedJavadocAction= new OpenAttachedJavadocAction(fSite);
					openAttachedJavadocAction.setSpecialSelectionProvider(selectionProvider);
					openAttachedJavadocAction.setImageDescriptor(JavaPluginImages.DESC_ELCL_OPEN_BROWSER);
					openAttachedJavadocAction.setDisabledImageDescriptor(JavaPluginImages.DESC_DLCL_OPEN_BROWSER);
					selectionProvider.addSelectionChangedListener(openAttachedJavadocAction);
					selectionProvider.setSelection(new StructuredSelection());
					tbm.add(openAttachedJavadocAction);
				}
				
				IInputChangedListener inputChangeListener= new IInputChangedListener() {
					public void inputChanged(Object newInput) {
						backAction.update();
						forwardAction.update();
						if (newInput == null) {
							selectionProvider.setSelection(new StructuredSelection());
						} else if (newInput instanceof BrowserInformationControlInput) {
							BrowserInformationControlInput input= (BrowserInformationControlInput) newInput;
							Object inputElement= input.getInputElement();
							selectionProvider.setSelection(new StructuredSelection(inputElement));
							boolean isJavaElementInput= inputElement instanceof IJavaElement;
							showInJavadocViewAction.setEnabled(isJavaElementInput);
							openDeclarationAction.setEnabled(isJavaElementInput);
						}
					}
				};
				iControl.addInputChangeListener(inputChangeListener);

				tbm.update(true);

				addLinkListener(iControl);
				return iControl;

			} else {
				return new DefaultInformationControl(parent, true);
			}
		}
	}

	private static void addLinkListener(final BrowserInformationControl control) {
		control.addLocationListener(new LocationListener() {
			@Override
			public void changing(LocationEvent event) {
				String loc= event.location;

				if ("about:blank".equals(loc)) { //$NON-NLS-1$
					return;
				}

				event.doit= false;

				if (loc.startsWith("about:")) { //$NON-NLS-1$
					// Relative links should be handled via head > base tag.
					// If no base is available, links just won't work.
					return;
				}

				URI uri= null;
				try {
					uri= new URI(loc);
				} catch (URISyntaxException e) {
					ElUiPlugin.getDefault().logError(e);
					return;
				}

				String scheme= uri == null ? null : uri.getScheme();
				if (JavaElementLinks.JAVADOC_VIEW_SCHEME.equals(scheme)) {
					IJavaElement linkTarget= JavaElementLinks.parseURI(uri);
					if (linkTarget == null)
						return;

					handleJavadocViewLink(linkTarget);
				} else if (JavaElementLinks.JAVADOC_SCHEME.equals(scheme)) {
					IJavaElement linkTarget= JavaElementLinks.parseURI(uri);
					if (linkTarget == null)
						return;

					handleInlineJavadocLink(linkTarget);
				} else if (JavaElementLinks.OPEN_LINK_SCHEME.equals(scheme)) {
					IJavaElement linkTarget= JavaElementLinks.parseURI(uri);
					if (linkTarget == null)
						return;

					handleDeclarationLink(linkTarget);
				} else if (scheme != null && (scheme.toLowerCase().equalsIgnoreCase("http") || //$NON-NLS-1$
						scheme.toLowerCase().equalsIgnoreCase("https"))) { //$NON-NLS-1$
					try {
						if (handleExternalLink(new URL(loc), event.display, true))
							return;
	
						event.doit= true;
					} catch (MalformedURLException e) {
						ElUiPlugin.getDefault().logError(e);
					}
				}
			}

			/* (non-Javadoc)
			 * @see org.eclipse.jdt.internal.ui.viewsupport.JavaElementLinks.ILinkHandler#handleJavadocViewLink(org.eclipse.jdt.core.IJavaElement)
			 */
			public void handleJavadocViewLink(IJavaElement linkTarget) {
				control.notifyDelayedInputChange(null);
				control.setVisible(false);
				control.dispose();
				try {
					JavadocView view= (JavadocView) JavaPlugin.getActivePage().showView(JavaUI.ID_JAVADOC_VIEW);
					view.setInput(linkTarget);
				} catch (PartInitException e) {
					ElUiPlugin.getDefault().logError(e);
				}
			}

			/* (non-Javadoc)
			 * @see org.eclipse.jdt.internal.ui.viewsupport.JavaElementLinks.ILinkHandler#handleInlineJavadocLink(org.eclipse.jdt.core.IJavaElement)
			 */
			public void handleInlineJavadocLink(IJavaElement linkTarget) {
				JavadocBrowserInformationControlInput hoverInfo= JavadocHover.getHoverInfo(new IJavaElement[] { linkTarget }, null, null, (JavadocBrowserInformationControlInput) control.getInput());
				if (control.hasDelayedInputChangeListener())
					control.notifyDelayedInputChange(hoverInfo);
				else
					control.setInput(hoverInfo);
			}

			/* (non-Javadoc)
			 * @see org.eclipse.jdt.internal.ui.viewsupport.JavaElementLinks.ILinkHandler#handleDeclarationLink(org.eclipse.jdt.core.IJavaElement)
			 */
			public void handleDeclarationLink(IJavaElement linkTarget) {
				control.notifyDelayedInputChange(null);
				control.dispose(); //FIXME: should have protocol to hide, rather than dispose
				try {
					JavadocHover.openDeclaration(linkTarget);
				} catch (PartInitException e) {
					ElUiPlugin.getDefault().logError(e);
				} catch (JavaModelException e) {
					ElUiPlugin.getDefault().logError(e);
				}
			}

			boolean handleExternalLink(URL url, Display display, boolean openInExternalBrowser) {
				control.notifyDelayedInputChange(null);
				control.dispose();

				// Open attached Javadoc links
				if (openInExternalBrowser)
					OpenBrowserUtil.openExternal(url, display);
				else 
					OpenBrowserUtil.open(url, display);
				
				return true;
			}

			public void changed(LocationEvent event) {
			}
		});
	}

	/**
	 * Action to go back to the previous input in the hover control.
	 *
	 * @since 3.4
	 */
	private static final class BackAction extends Action {
		private final BrowserInformationControl fInfoControl;

		public BackAction(BrowserInformationControl infoControl) {
			fInfoControl= infoControl;
			setText(Messages.ELInfoHover_back);
			ISharedImages images= PlatformUI.getWorkbench().getSharedImages();
			setImageDescriptor(images.getImageDescriptor(ISharedImages.IMG_TOOL_BACK));
			setDisabledImageDescriptor(images.getImageDescriptor(ISharedImages.IMG_TOOL_BACK_DISABLED));

			update();
		}

		@Override
		public void run() {
			BrowserInformationControlInput previous= (BrowserInformationControlInput) fInfoControl.getInput().getPrevious();
			if (previous != null) {
				fInfoControl.setInput(previous);
			}
		}

		public void update() {
			BrowserInformationControlInput current= fInfoControl.getInput();

			if (current != null && current.getPrevious() != null) {
				BrowserInput previous= current.getPrevious();
				setToolTipText(org.eclipse.jdt.internal.corext.util.Messages.format(Messages.ELInfoHover_back_toElement_toolTip, BasicElementLabels.getJavaElementName(previous.getInputName())));
				setEnabled(true);
			} else {
				setToolTipText(Messages.ELInfoHover_back);
				setEnabled(false);
			}
		}
	}

	/**
	 * Action to go forward to the next input in the hover control.
	 *
	 * @since 3.4
	 */
	private static final class ForwardAction extends Action {
		private final BrowserInformationControl fInfoControl;

		public ForwardAction(BrowserInformationControl infoControl) {
			fInfoControl= infoControl;
			setText(Messages.ELInfoHover_forward);
			ISharedImages images= PlatformUI.getWorkbench().getSharedImages();
			setImageDescriptor(images.getImageDescriptor(ISharedImages.IMG_TOOL_FORWARD));
			setDisabledImageDescriptor(images.getImageDescriptor(ISharedImages.IMG_TOOL_FORWARD_DISABLED));

			update();
		}

		@Override
		public void run() {
			BrowserInformationControlInput next= (BrowserInformationControlInput) fInfoControl.getInput().getNext();
			if (next != null) {
				fInfoControl.setInput(next);
			}
		}

		public void update() {
			BrowserInformationControlInput current= fInfoControl.getInput();

			if (current != null && current.getNext() != null) {
				setToolTipText(org.eclipse.jdt.internal.corext.util.Messages.format(Messages.ELInfoHover_forward_toElement_toolTip, BasicElementLabels.getJavaElementName(current.getNext().getInputName())));
				setEnabled(true);
			} else {
				setToolTipText(Messages.ELInfoHover_forward_toolTip);
				setEnabled(false);
			}
		}
	}

	/**
	 * Action that shows the current hover contents in the Javadoc view.
	 *
	 * @since 3.4
	 */
	private static final class ShowInJavadocViewAction extends Action {
		private final BrowserInformationControl fInfoControl;

		public ShowInJavadocViewAction(BrowserInformationControl infoControl) {
			fInfoControl= infoControl;
			setText(Messages.ELInfoHover_showInJavadoc);
			setImageDescriptor(JavaPluginImages.DESC_OBJS_JAVADOCTAG); //TODO: better image
		}

		/*
		 * @see org.eclipse.jface.action.Action#run()
		 */
		@Override
		public void run() {
			JavadocBrowserInformationControlInput infoInput= (JavadocBrowserInformationControlInput) fInfoControl.getInput(); //TODO: check cast
			fInfoControl.notifyDelayedInputChange(null);
			fInfoControl.dispose(); //FIXME: should have protocol to hide, rather than dispose
			try {
				JavadocView view= (JavadocView) JavaPlugin.getActivePage().showView(JavaUI.ID_JAVADOC_VIEW);
				view.setInput(infoInput);
			} catch (PartInitException e) {
				ElUiPlugin.getDefault().logError(e);
			}
		}
	}

	/**
	 * Action that opens the current hover input element.
	 *
	 * @since 3.4
	 */
	private static final class OpenDeclarationAction extends Action {
		private final BrowserInformationControl fInfoControl;

		public OpenDeclarationAction(BrowserInformationControl infoControl) {
			fInfoControl= infoControl;
			setText(Messages.ELInfoHover_openDeclaration);
			JavaPluginImages.setLocalImageDescriptors(this, "goto_input.gif"); //$NON-NLS-1$ //TODO: better images
		}

		/*
		 * @see org.eclipse.jface.action.Action#run()
		 */
		@Override
		public void run() {
			JavadocBrowserInformationControlInput infoInput= (JavadocBrowserInformationControlInput) fInfoControl.getInput(); //TODO: check cast
			fInfoControl.notifyDelayedInputChange(null);
			fInfoControl.dispose(); //FIXME: should have protocol to hide, rather than dispose

			try {
				JavadocHover.openDeclaration(infoInput.getElement());
			} catch (PartInitException e) {
				ElUiPlugin.getDefault().logError(e);
			} catch (JavaModelException e) {
				ElUiPlugin.getDefault().logError(e);
			}
		}
	}
}
