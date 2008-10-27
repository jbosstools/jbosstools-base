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
package org.jboss.tools.common.text.ext.hyperlink.xml;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.StringTokenizer;
import java.util.zip.ZipFile;

import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.pde.internal.ui.editor.JarEntryEditorInput;
import org.eclipse.pde.internal.ui.editor.JarEntryFile;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPersistableElement;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.editors.text.ILocationProvider;
import org.eclipse.wst.xml.core.internal.XMLCorePlugin;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMAttr;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlink;
import org.jboss.tools.common.text.ext.hyperlink.xpl.Messages;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 * @author Jeremy
 */
public class XMLXmlNsHyperlink extends AbstractHyperlink {
	
	
    class StorageEditorInput extends PlatformObject implements IStorageEditorInput, IStorage, ILocationProvider {
        private File fFile;
        
        public StorageEditorInput(File file) {
            fFile = file;
        }
        
        public IStorage getStorage() {
            return (fFile == null ? null : this);
        }

        public ImageDescriptor getImageDescriptor() {
            return null;
        }

        public String getName() {
			return fFile.getName();
        }

        public IPersistableElement getPersistable() {
            return null;
        }

        public String getToolTipText() {
            return getFullPath().toOSString();
        }
        
		/*
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		public boolean equals(Object o) {
			if (o == this)
				return true;

			if (o instanceof StorageEditorInput) {
				StorageEditorInput input = (StorageEditorInput) o;
				return fFile.equals(input.fFile);
			}

			return false;
		}

        public int hashCode() {
            return fFile.hashCode();
        }

        public boolean exists() {
            return fFile.exists();
        }

		public InputStream getContents() throws CoreException {
			try {
				return new FileInputStream(fFile);
			} catch (FileNotFoundException x) {
				//ignore
				return null;
			}
		}

		public IPath getFullPath() {
			return new Path (fFile.getAbsolutePath());
		}

		public boolean isReadOnly() {
			return !fFile.canWrite();
			
		}

		public IPath getPath(Object element) {
			if (element instanceof StorageEditorInput) {
				return new Path(((StorageEditorInput)element).fFile.getAbsolutePath());
			}
			return null;
		}
    }
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doHyperlink(org.eclipse.jface.text.IRegion)
	 */
	protected void doHyperlink(IRegion region) {
	
		try {
//			IFile f = getFileFromCatalog(getURI(region));
//			IFile f = getFileFromCatalog(getPublicId(region), getSystemId(region));

			String mappedSystemId = getMappedSystemIdFromCatalog(getPublicId(region), getSystemId(region));
			String filename = getFilenameFromMappedSystemId(mappedSystemId);
			IEditorPart part = null;
			if (filename != null) {
//				openFileInEditor(filename);
				part = openExternalFile(filename);
				if (part == null)
					openFileFailed();
			} else if (mappedSystemId != null) {
				openFileInEditor(mappedSystemId);
			} else {
				String uri = getURI(region);
				if (uri != null && uri.toLowerCase().startsWith("http:")) {
					URL url = null;
					url = new URL(uri);
					IWorkbenchBrowserSupport browserSupport = PlatformUI.getWorkbench().getBrowserSupport();
					IWebBrowser browser = browserSupport.createBrowser(IWorkbenchBrowserSupport.LOCATION_BAR | IWorkbenchBrowserSupport.NAVIGATION_BAR, null, null, null);
					browser.openURL(url);
				} else {
					openFileFailed();
				}
			}
		} catch (MalformedURLException x) {
			// could not open editor
			openFileFailed();
		} catch (PartInitException x) {
			openFileFailed();
		}
	}
	
	/**
	 * Try to open the external file, fileString in its default editor
	 * @param fileString
	 * @return IEditorPart editor opened or null if editor could not be opened
	 */
	protected IEditorPart openExternalFile(String fileString) {
		// file does not exist in workspace so try to open using system editor
		File file = new File(fileString);
		if (!file.exists())
			return null;
		
		IEditorInput input = new StorageEditorInput(file);
		String editorId = getEditorId(fileString);
		
		try {
			IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
			return page.openEditor(input, editorId, true);
		}
		catch (PartInitException pie) {
			ExtensionsPlugin.getPluginLog().logError(pie);
		}
		return null;
	}

	protected final String JAR_FILE_PROTOCOL = "jar:file:/";//$NON-NLS-1$
	protected final String JAR_FILE = "jar:file:";//$NON-NLS-1$
	private int taglibLength;
	private int taglibOffset;
	
    /* (non-Javadoc)
     * @see com.ibm.sse.editor.hyperlink.AbstractHyperlink#openFileInEditor(java.lang.String)
     */
    protected void openFileInEditor(String fileString) {
        if (fileString.startsWith(JAR_FILE_PROTOCOL)) {
			fileString = fileString.substring(JAR_FILE_PROTOCOL.length());
			IEditorInput jarEditorInput = createEditorInput(fileString);
	        IEditorPart part = openFileInEditor(jarEditorInput,  fileString);
	        if (part == null) openFileFailed();
        } else if (fileString.startsWith(JAR_FILE)) {
				fileString = fileString.substring(JAR_FILE.length());
				IEditorInput jarEditorInput = createEditorInputAlternate(fileString);
		        IEditorPart part = openFileInEditor(jarEditorInput,  fileString);
		        if (part == null) openFileFailed();
		} else {
			super.openFileInEditor(fileString);    
		}
    }

	protected IEditorInput createEditorInputAlternate(String fileString) {
		String jarName = fileString.substring(0,fileString.indexOf("!"));
		String entryName = fileString.substring(fileString.indexOf("!")+2,fileString.length());

		ZipFile jarFile = null;
		try {
			jarFile = new ZipFile(jarName);
		} catch (IOException e) {
			return null;
		}
		JarEntryFile jarEntryFile = new JarEntryFile(jarFile, entryName); 

		JarEntryEditorInput jarEditorInput = new JarEntryEditorInput(jarEntryFile) {
        	public boolean equals(Object arg) {
        		return this.toString().equals(arg.toString());
        	}
        };
        return jarEditorInput;
	}

	String getURI(IRegion region) {
		try {
			return Utils.trimQuotes(getDocument().get(region.getOffset(), region.getLength()));
		} catch (BadLocationException x) {
			// Ignore
			return null;
		}
		
	}

	protected String getPublicId(IRegion region) {
		String text = getURI(region);
		if (text == null) return null;
		int spacer = text.indexOf(" ");
		if (spacer == -1) spacer = text.indexOf("\t");
		return (spacer == -1 ? text : text.substring(0, spacer));
	}

	protected String getSystemId(IRegion region) {
		String text = getURI(region);
		if (text == null) return null;
		int spacer = text.indexOf(" ");
		if (spacer == -1) spacer = text.indexOf("\t");
		if (spacer == -1) spacer = 0;
		
		return text.substring(spacer).trim();
	}

	private String getMappedSystemIdFromCatalog(String publicId, String systemId) {
		String mappedSystemId = null;
		try {
			if (publicId != null)
	        	mappedSystemId = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolvePublic(publicId, systemId);
			if (mappedSystemId == null)
				mappedSystemId = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolveSystem(systemId);
			if (mappedSystemId == null && systemId != null)
	        	mappedSystemId = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolveURI(systemId);
		} catch (IOException e) {
			// Ignore
			return null;		
		}
		return mappedSystemId;
	}
	
	private String getFilenameFromMappedSystemId(String mappedSystemId) {
		String fileName = null;
		try {
	        if(mappedSystemId!=null) {
	            File file = new File(new URL(mappedSystemId).getFile());
	            if(file.isFile()) {
	                fileName = file.getAbsolutePath();
	            }
	        }
		} catch (MalformedURLException x) {
			// Ignore
			return null;
		}
		return fileName;
	}

	IRegion fLastRegion = null;
	/** 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doGetHyperlinkRegion(int)
	 */
	protected IRegion doGetHyperlinkRegion(int offset) {
		fLastRegion = getRegion(offset);
		return fLastRegion;
	}
	
	protected IRegion getRegion(int offset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, offset);
			if (!(n instanceof IDOMAttr)) return null; 
			IDOMAttr xmlnsAttr = (IDOMAttr)n;
			if (xmlnsAttr.getName() == null || 
					(!xmlnsAttr.getName().equals("xmlns") &&
					!xmlnsAttr.getName().startsWith("xmlns:") &&
					!xmlnsAttr.getName().endsWith(":schemaLocation")
					)) return null;
			
			String text = xmlnsAttr.getValueRegionText();
			String value = Utils.trimQuotes(xmlnsAttr.getNodeValue());
			int start = xmlnsAttr.getValueRegionStartOffset();
			
			taglibLength = value.length();
			taglibOffset = start + text.indexOf(value);
			
			StringTokenizer tokenizer = new StringTokenizer(value);
			
			int newOffset = -1;
			int newLength = -1;
			if (tokenizer.countTokens() > 1) {
				while (tokenizer.hasMoreTokens()) {
					String next = tokenizer.nextToken();
					int nextStart = taglibOffset + value.indexOf(next);
					if (offset >= nextStart
							&& offset <= nextStart + next.length()) {
						newOffset = nextStart;
						newLength = next.length();
						break;
					}
				}
			}
			if (newOffset != -1) {
				taglibLength = newLength;
				taglibOffset = newOffset;
			}

			return new Region(taglibOffset,taglibLength);
		} finally {
			smw.dispose();
		}

	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see IHyperlink#getHyperlinkText()
	 */
	public String getHyperlinkText() {
		String uri = getURI(fLastRegion);
		if (uri == null)
			return  MessageFormat.format(Messages.NotFound, "URI");
		
		return MessageFormat.format(Messages.Open, uri);
	}

}
