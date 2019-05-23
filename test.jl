struct Nani {
	int ass;
};

typedef struct Nani* shindeiru;

int main() {
	shindeiru foo = new Nani;
	foo->ass = 1;
	return 0;
}
