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
import java.io.InputStream;
import java.net.URL;

import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.jdt.internal.core.JarEntryFile;
import org.eclipse.jdt.internal.ui.javaeditor.JarEntryEditorInput;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.IRegion;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPersistableElement;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.editors.text.ILocationProvider;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMAttr;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlink;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;

import org.eclipse.wst.xml.core.internal.XMLCorePlugin;

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
			} catch (Exception x) {
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
			
			String filename = getFilenameFromCatalog(getPublicId(region), getSystemId(region));
			IEditorPart part = null;
			if (filename != null) {
//				openFileInEditor(filename);
				part = openExternalFile(filename);
			}  
			if (part == null)
				openFileFailed();

		} catch (Exception x) {
			// could not open editor
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
	
    /* (non-Javadoc)
     * @see com.ibm.sse.editor.hyperlink.AbstractHyperlink#openFileInEditor(java.lang.String)
     */
    protected void openFileInEditor(String fileString) {
        try {
	        if (fileString.startsWith(JAR_FILE_PROTOCOL)) {
				fileString = fileString.substring(JAR_FILE_PROTOCOL.length());
				IEditorInput jarEditorInput = createEditorInput(fileString);
		        IEditorPart part = openFileInEditor(jarEditorInput,  fileString);
		        if (part == null) openFileFailed();
			} else {
				super.openFileInEditor(fileString);    
			}
        } catch (Exception x) {
        	openFileFailed();
        }
    }
    
	String getURI(IRegion region) {
		try {
			return Utils.trimQuotes(getDocument().get(region.getOffset(), region.getLength()));
		} catch (Exception x) {
			//ignore
			return null;
		}
	}

	protected String getPublicId(IRegion region) {
		try {
			String text = getURI(region);
			if (text == null) return null;
			int spacer = text.indexOf(" ");
			if (spacer == -1) spacer = text.indexOf("\t");
			return (spacer == -1 ? null : text.substring(0, spacer));
		} catch (Exception x) {
			//ignore
			return null;
		}
	}

	protected String getSystemId(IRegion region) {
		try {
			String text = getURI(region);
			if (text == null) return null;
			int spacer = text.indexOf(" ");
			if (spacer == -1) spacer = text.indexOf("\t");
			if (spacer == -1) spacer = 0;
			
			return text.substring(spacer).trim();
		} catch (Exception x) {
			//ignore
			return null;
		}
	}

	private String getFilenameFromCatalog(String publicId, String systemId) {
//		IStructuredModel model = null;
		
		try {
			String mappedSystemId = null;

			if (publicId != null)
	        	mappedSystemId = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolvePublic(publicId, systemId);
			if (mappedSystemId == null)
				mappedSystemId = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolveSystem(systemId);
			if(mappedSystemId == null) return null;
	        
			String fileName = null;
	        
	        if(mappedSystemId!=null) {
	            try {
		            File file = new File(new URL(mappedSystemId).getFile());
		            if(file.isFile()) {
		                fileName = file.getAbsolutePath();
		            }
	            } catch(Exception e) {
//	        		ExtensionsPlugin.log(e);
	            }
	        }
	        
			return fileName;
		} catch (Exception x) {
			//ignore
			return null;
		} finally {
		}
	}


	/** 
	 * @seecom.ibm.sse.editor.AbstractHyperlink#doGetHyperlinkRegion(int)
	 */
	protected IRegion doGetHyperlinkRegion(int offset) {
		IRegion region = getRegion(offset);
		return region;
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
			
			final int taglibLength = value.length();
			final int taglibOffset = start + text.indexOf(value);
			
			IRegion region = new IRegion () {
				public int getLength() {
					return taglibLength;
				}

				public int getOffset() {
					return taglibOffset;
				}
				
				public boolean equals(Object arg) {
					if (!(arg instanceof IRegion)) return false;
					IRegion region = (IRegion)arg;
					
					if (getOffset() != region.getOffset()) return false;
					if (getLength() != region.getLength()) return false;
					return true;
				}

			};
			return region;

		} catch (Exception x) {
			ExtensionsPlugin.getPluginLog().logError("Error while obtaining region", x);
			return null;
		} finally {
			smw.dispose();
		}

	}

}
