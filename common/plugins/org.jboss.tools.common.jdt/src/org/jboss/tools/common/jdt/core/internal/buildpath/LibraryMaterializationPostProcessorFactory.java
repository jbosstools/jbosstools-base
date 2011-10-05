package org.jboss.tools.common.jdt.core.internal.buildpath;

import org.jboss.tools.common.jdt.core.buildpath.ILibraryMaterializationPostProcessor;
import org.jboss.tools.common.jdt.core.buildpath.ILibraryMaterializationPostProcessorFactory;

public class LibraryMaterializationPostProcessorFactory implements ILibraryMaterializationPostProcessorFactory {

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.jdt.core.buildpath.ILibraryMaterializationPostProcessorFactory#getLibraryMaterializationPostProcessors()
	 */
	@Override
	public ILibraryMaterializationPostProcessor[] getLibraryMaterializationPostProcessors() {
		return new ILibraryMaterializationPostProcessor[]{new MavenLibraryMaterializationPostProcessor()};
	}

}
