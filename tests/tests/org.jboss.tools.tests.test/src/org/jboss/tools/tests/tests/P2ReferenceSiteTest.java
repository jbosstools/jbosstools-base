package org.jboss.tools.tests.tests;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.equinox.p2.repository.IRepositoryReference;
import org.eclipse.equinox.p2.repository.metadata.IMetadataRepository;
import org.eclipse.equinox.p2.repository.metadata.IMetadataRepositoryManager;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

public class P2ReferenceSiteTest extends P2RepositoryTest {

	@Parameters(name = "{0} ref@{1}")
	static public Collection<Object[]> getReferenceLocations() throws URISyntaxException, IOException {

		IMetadataRepositoryManager repositoryManager = Activator
				.getRepositoryManager();

		List<Object[]> result = new ArrayList<Object[]>();
		
		Map<String, List<String>> entries = new HashMap<String, List<String>>();
		for(String rootLocation : getRootLocations()) {

			ArrayList<IStatus> errors = new ArrayList<IStatus>();
			HashMap<URI, IMetadataRepository> allrepositories = new HashMap<URI, IMetadataRepository>();

			 IMetadataRepository repository = loadRepository(repositoryManager, allrepositories,
					new URI(rootLocation), false, errors, textProgressMontior());
			 
			 for(IRepositoryReference ref : repository.getReferences()) {
				 List<String> existing = entries.get(ref.getLocation().toString());
				 if(existing==null) {
					 existing = new ArrayList<String>();
				 }
				 existing.add(rootLocation);
				 entries.put(ref.getLocation().toString(), existing);				 
			 }	 
		}		
		
		for(Entry<String, List<String>> entry: entries.entrySet()) {
		 result.add(new Object[] { entry.getKey(), entry.getValue() });
		}
		return result;
	}
				
	@Parameter(value=1)
	public List<String> rootLocations;
	
}	

