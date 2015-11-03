package org.jboss.tools.foundation.core.test.credentials;

import org.jboss.tools.foundation.core.credentials.CredentialAdapter;
import org.jboss.tools.foundation.core.credentials.CredentialService;
import org.jboss.tools.foundation.core.credentials.ICredentialDomain;
import org.jboss.tools.foundation.core.credentials.ICredentialListener;
import org.jboss.tools.foundation.core.credentials.ICredentialsModel;
import org.junit.Test;

import junit.framework.TestCase;
public class CredentialsModelTest extends TestCase {
	
	@Test
	public void testInitialization() {
		ICredentialsModel m = CredentialService.getCredentialModel();
		assertNotNull(m);
		ICredentialDomain[] domains = m.getDomains();
		assertEquals(2, domains.length);
		assertNotNull(m.getDomain("access.redhat.com"));
		assertNotNull(m.getDomain("jboss.org"));
		assertFalse(domains[0].getRemovable());
		assertFalse(domains[1].getRemovable());
	}
	
	@Test
	public void testAddRemoveDomain() {
		ICredentialsModel m = CredentialService.getCredentialModel();
		assertNotNull(m);
		
		final Boolean[] checkAdded = new Boolean[]{false};
		final Boolean[] checkRemoved = new Boolean[]{false};
		
		ICredentialListener listener = new CredentialAdapter() {
			public void domainRemoved(ICredentialDomain domain) {
				checkRemoved[0] = true;
			}
			public void domainAdded(ICredentialDomain domain) {
				checkAdded[0] = true;
			}
		};
		m.addCredentialListener(listener);
		
		try {
			ICredentialDomain[] domains = m.getDomains();
			assertEquals(2, domains.length);
			ICredentialDomain added = m.addDomain("test1", "test1", true);
			domains = m.getDomains();
			assertEquals(3, domains.length);
			m.removeDomain(added);
			domains = m.getDomains();
			assertEquals(2, domains.length);
			
			assertTrue(checkAdded[0]);
			assertTrue(checkRemoved[0]);
		} finally {
			m.removeCredentialListener(listener);
		}
	}
	
	
	@Test 
	public void testAddRemoveCredential() {
		ICredentialsModel m = CredentialService.getCredentialModel();
		assertNotNull(m);

		final Boolean[] checkAdded = new Boolean[]{false};
		final Boolean[] checkRemoved = new Boolean[]{false};

		ICredentialListener listener = new CredentialAdapter() {
			@Override
			public void credentialAdded(ICredentialDomain domain, String user) {
				checkAdded[0] = true;
			}
			@Override
			public void credentialRemoved(ICredentialDomain domain, String user) {
				checkRemoved[0] = true;
			}
		};
		m.addCredentialListener(listener);
		
		try {
			ICredentialDomain d = m.getDomain("jboss.org");
			String[] users = d.getUsernames();
			assertNotNull(users);
			assertEquals(0, users.length);
			m.addCredentials(d, "username", "p4ssw0rD");
			users = d.getUsernames();
			assertNotNull(users);
			assertEquals(1, users.length);
			
			m.removeCredentials(d, "username");
			users = d.getUsernames();
			assertNotNull(users);
			assertEquals(0, users.length);
			
			assertTrue(checkAdded[0]);
			assertTrue(checkRemoved[0]);
		} finally {
			m.removeCredentialListener(listener);
		}
	}
	

	@Test 
	public void testChangePassword() {

		ICredentialsModel m = CredentialService.getCredentialModel();
		assertNotNull(m);

		final Boolean[] checkChanged = new Boolean[]{false};

		ICredentialListener listener = new CredentialAdapter() {
			public void credentialChanged(ICredentialDomain domain, String user) {
				checkChanged[0] = true;
			}
		};
		m.addCredentialListener(listener);
		
		try {
			ICredentialDomain d = m.getDomain("jboss.org");
			String[] users = d.getUsernames();
			assertNotNull(users);
			assertEquals(0, users.length);
			m.addCredentials(d, "username", "p4ssw0rD");
			users = d.getUsernames();
			assertNotNull(users);
			assertEquals(1, users.length);
			
			// Add the same user again
			m.addCredentials(d, "username", "p4ssw0rD");
			users = d.getUsernames();
			assertNotNull(users);
			assertEquals(1, users.length);
			// credential changed will be fired even if the password is the same
			assertTrue(checkChanged[0]); 
			checkChanged[0] = true;
			
			m.addCredentials(d, "username", "n3wPASS");
			users = d.getUsernames();
			assertNotNull(users);
			assertEquals(1, users.length);
			assertTrue(checkChanged[0]); // password changed should be called
			
			m.removeCredentials(d, "username");
			users = d.getUsernames();
			assertNotNull(users);
			assertEquals(0, users.length);
			
			assertTrue(checkChanged[0]);
		} finally {
			m.removeCredentialListener(listener);
		}
	}
	

	@Test 
	public void testDefaultUser() {

		ICredentialsModel m = CredentialService.getCredentialModel();
		assertNotNull(m);

		final Boolean[] checkDefaultChanged = new Boolean[]{false};

		ICredentialListener listener = new CredentialAdapter() {
			public void defaultUsernameChanged(ICredentialDomain domain, String user) {
				checkDefaultChanged[0] = true;
			}
		};
		m.addCredentialListener(listener);
		
		try {
			ICredentialDomain d = m.getDomain("jboss.org");
			String[] users = d.getUsernames();
			assertNotNull(users);
			assertEquals(0, users.length);
			m.addCredentials(d, "username", "p4ssw0rD");
			users = d.getUsernames();
			assertNotNull(users);
			assertEquals(1, users.length);
			
			assertTrue(checkDefaultChanged[0]);
			assertEquals("username", d.getDefaultUsername());
			checkDefaultChanged[0] = false;
			
			// Add the a different user 
			m.addCredentials(d, "username2", "p4ssw0rD");
			users = d.getUsernames();
			assertNotNull(users);
			assertEquals(2, users.length);
			assertFalse(checkDefaultChanged[0]);
			assertEquals("username", d.getDefaultUsername());
			
			m.setDefaultCredential(d, "username");
			assertFalse(checkDefaultChanged[0]); // didnt change the default, so no event
			
			m.setDefaultCredential(d, "username2");
			assertTrue(checkDefaultChanged[0]); 
			assertEquals("username2", d.getDefaultUsername());
			checkDefaultChanged[0] = false;
			
			
			m.removeCredentials(d, "username2"); // Remove the current default, verify default changed
			assertTrue(checkDefaultChanged[0]); 
			users = d.getUsernames();
			assertNotNull(users);
			assertEquals(1, users.length);
			
			m.removeCredentials(d, "username");
			users = d.getUsernames();
			assertNotNull(users);
			assertEquals(0, users.length);
		} finally {
			m.removeCredentialListener(listener);
		}
	}
}
